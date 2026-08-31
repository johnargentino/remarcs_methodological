
f_Xt = function(n_vec){
  #readline("missing days must be represented with zeros not NA's")
  X_t = matrix(0,
               nrow = sum(n_vec),
               ncol = length(n_vec))
  trackrow = 1
  for (i in 1:length(n_vec)){
    if(n_vec[i] > 0){
      X_t[trackrow:(trackrow + n_vec[i] - 1), i] = 1
      trackrow = trackrow + n_vec[i]
    }
  }
  return(X_t)
}

f_Xt_sparse = function(n_vec){
  #readline("missing days must be represented with zeros not NA's")

  TT <- length(n_vec)
  total_rows <- sum(n_vec)

  # Column index for each row
  j <- rep(seq_len(TT), times = n_vec)

  # Row index
  i <- seq_len(total_rows)

  # Values (all ones)
  x <- rep(1, total_rows)

  sparseMatrix(
    i = i,
    j = j,
    x = x,
    dims = c(total_rows, TT)
  )
}

conf_region_2d <- function(alpha = .05,
                           theta_hat,
                           theta_true,
                           Sigma,
                           n_points = 200,
                           level_label = TRUE
) {
  stopifnot(
    length(theta_hat) == 2,
    is.matrix(Sigma),
    all(dim(Sigma) == c(2, 2))
  )

  # Chi-square cutoff
  r2 <- qchisq(alpha, df = 3)

  # Eigen decomposition
  eig <- eigen(Sigma)
  A <- eig$vectors %*% diag(sqrt(eig$values))

  # Parametric circle
  t <- seq(0, 2 * pi, length.out = n_points)
  circle <- rbind(cos(t), sin(t))

  # Ellipse
  ellipse <- t(theta_hat + sqrt(r2) * A %*% circle)

  df_ellipse <- data.frame(
    x = ellipse[, 1],
    y = ellipse[, 2]
  )

  df_point <- data.frame(
    x = theta_hat[1],
    y = theta_hat[2]
  )



  p <- ggplot(df_ellipse, aes(x, y)) +
    geom_path(linewidth = 1) +
    geom_point(data = df_point, aes(x, y), size = 3, col = "red") +
    geom_point(data = tibble(x = theta_true[1], y = theta_true[2]), mapping = aes(x,y), col = "blue") +
    coord_equal() +
    theme_minimal() +
    labs(
      x = expression(theta[1]),
      y = expression(theta[2]),
      title = if (level_label)
        paste0(round(alpha * 100), "% Confidence Region")
      else NULL
    )

  return(p)
}

simulate_and_plot_conf_o <- function(phi, theta, var_eta, TT, sims,
                                     var_eps, nn, alpha = 0.05,
                                     ngrid = 40) {

  # --- Step 1: compute theoretical covariance ---
  theta_b = fwd(phi, theta, var_eta, var_eps, nn)[1]
  var_b = fwd(phi, theta, var_eta, var_eps, nn)[2]

  Sigma_inv = solve(Sigma_o(ar = phi, ma_b = theta_b,
                            veps = var_eps, v_b = var_b, n = nn)[1:3,1:3])
  true_vec = c(phi, theta_b, var_b)
  chi_q = qchisq(1 - alpha, df = 3)

  # --- Step 2: simulate estimates ---
  est_matrix = matrix(NA, nrow = sims, ncol = 3)
  inside = logical(sims)

  for (sim in 1:sims) {
    # ARMA simulation
    test_u = arima.sim(model = list(ar = phi, ma = theta), sd = sqrt(var_eta), n = TT)
    eps = rnorm(n = TT * nn, mean = 0, sd = sqrt(var_eps))

    # Averaging over daily observations
    y = rep(test_u, each = nn) + eps
    w = t(matrix(y, nrow = nn)) %*% rep(1/nn, nn)

    # Variance estimation
    x = y - rep(w, each = nn)
    var_eps_hat = sum(x ^ 2) / ((nn - 1) * TT)

    # ARMA fit
    arma_noise = arima(w, order = c(1,0,1), include.mean = FALSE)
    phi_hat = arma_noise$coef["ar1"]
    theta_b_hat = arma_noise$coef["ma1"]
    var_b_hat = arma_noise$sigma2

    # Store vector
    est_vec = c(phi_hat, theta_b_hat, var_b_hat)
    est_matrix[sim, ] = est_vec

    # Chi-square check
    chi_stat = t(est_vec - true_vec) %*% Sigma_inv %*% (est_vec - true_vec) * TT
    inside[sim] = chi_stat < chi_q
  }

  # --- Step 3: compute confidence ellipsoid grid ---
  Sigma = solve(Sigma_inv) / TT
  r2 = qchisq(1 - alpha, df = 3)
  eig = eigen(Sigma)
  A = eig$vectors %*% diag(sqrt(eig$values * r2))

  # Sphere parameterization
  u = seq(0, 2*pi, length.out = ngrid)
  v = seq(0, pi, length.out = ngrid)
  x = outer(cos(u), sin(v))
  y = outer(sin(u), sin(v))
  z = outer(rep(1, length(u)), cos(v))

  ellipsoid = A %*% rbind(as.vector(x), as.vector(y), as.vector(z))
  x_e = matrix(ellipsoid[1, ] + true_vec[1], ngrid, ngrid)
  y_e = matrix(ellipsoid[2, ] + true_vec[2], ngrid, ngrid)
  z_e = matrix(ellipsoid[3, ] + true_vec[3], ngrid, ngrid)

  # --- Step 4: generate plot ---
  p <- plot_ly() %>%
    add_surface(
      x = x_e, y = y_e, z = z_e,
      opacity = 0.5,
      name = paste0(round(100*(1-alpha)), "% confidence ellipsoid"),
      showscale = FALSE
    ) %>%
    add_markers(
      x = est_matrix[,1],
      y = est_matrix[,2],
      z = est_matrix[,3],
      color = ~inside,
      colors = c("red", "green"),
      marker = list(size = 3),
      name = "Simulations (green = inside)"
    ) %>%
    add_markers(
      x = true_vec[1], y = true_vec[2], z = true_vec[3],
      marker = list(size = 5, color = "blue"),
      name = "True value"
    ) %>%
    layout(
      scene = list(
        xaxis = list(title = "phi"),
        yaxis = list(title = "theta_b"),
        zaxis = list(title = "var_b")
      ),
      title = paste0(round(100*(1-alpha)), "% Confidence Ellipsoid with Simulations")
    )

  return(p)
}

simulate_and_plot_conf_l <- function(phi,
                                     theta,
                                     var_eta,
                                     TT,
                                     sims,
                                     var_eps,
                                     nn,
                                     alpha = 0.05,
                                     ngrid = 40) {

  # --- Step 1: compute theoretical covariance ---
  theta_b = fwd(phi, theta, var_eta, var_eps, nn)[1]
  var_b = fwd(phi, theta, var_eta, var_eps, nn)[2]
  true_vec = c(phi,theta,var_eta)
  S_o = Sigma_o(ar = phi, ma_b = theta_b,
                veps = var_eps, v_b = var_b, n = nn)
  dHdtau0 = dHf_dtau0(phi, theta_b, var_eps/n, var_b)
  dHdtau1 = dHf_dtau1(theta,var_eta)
  GGG = -solve(dHdtau1) %*% dHdtau0
  S_f = GGG %*% S_o %*% t(GGG)
  S_l = S_f[c(1,5,6), c(1,5,6)]
  Sigma = S_l
  chi_q = qchisq(1 - alpha, df = 3)

  # --- Step 2: simulate estimates ---
  est_matrix = matrix(NA, nrow = sims, ncol = 3)
  inside = logical(sims)

  for (sim in 1:sims) {
    # ARMA simulation
    test_u = arima.sim(model = list(ar = phi, ma = theta), sd = sqrt(var_eta), n = TT)
    eps = rnorm(n = TT * nn, mean = 0, sd = sqrt(var_eps))

    # Averaging over daily observations
    y = rep(test_u, each = nn) + eps
    w = t(matrix(y, nrow = nn)) %*% rep(1 / nn, nn)

    # Variance estimation
    x = y - rep(w, each = nn)
    var_eps_hat = sum(x ^ 2) / ((nn - 1) * TT)

    # ARMA fit
    arma_noise = arima(w, order = c(1,0,1), include.mean = FALSE)
    phi_hat = arma_noise$coef["ar1"]
    theta_b_hat = arma_noise$coef["ma1"]
    var_b_hat = arma_noise$sigma2
    wong_est = wong(ar_hat = phi_hat,
                    ma_b_hat = theta_b_hat,
                    v_b_hat = var_b_hat,
                    veps_hat = var_eps_hat,
                    n = nn)
    theta_hat = wong_est[1]
    var_eta_hat = wong_est[2]

    # Store vector
    est_vec = c(phi_hat, theta_hat, var_eta_hat)
    est_matrix[sim, ] = est_vec

    # Chi-square check
    chi_stat = t(est_vec - true_vec) %*% Sigma_inv %*% (est_vec - true_vec) * TT
    inside[sim] = chi_stat < chi_q
  }

  # --- Step 3: compute confidence ellipsoid grid ---
  Sigma =  Sigma / TT
  r2 = qchisq(1 - alpha, df = 3)
  eig = eigen(Sigma)
  A = eig$vectors %*% diag(sqrt(eig$values * r2))

  # Sphere parameterization
  u = seq(0, 2*pi, length.out = ngrid)
  v = seq(0, pi, length.out = ngrid)
  x = outer(cos(u), sin(v))
  y = outer(sin(u), sin(v))
  z = outer(rep(1, length(u)), cos(v))

  ellipsoid = A %*% rbind(as.vector(x), as.vector(y), as.vector(z))
  x_e = matrix(ellipsoid[1, ] + true_vec[1], ngrid, ngrid)
  y_e = matrix(ellipsoid[2, ] + true_vec[2], ngrid, ngrid)
  z_e = matrix(ellipsoid[3, ] + true_vec[3], ngrid, ngrid)

  # --- Step 4: generate plot ---
  p <- plot_ly() %>%
    add_surface(
      x = x_e, y = y_e, z = z_e,
      opacity = 0.5,
      name = paste0(round(100*(1-alpha)), "% confidence ellipsoid"),
      showscale = FALSE
    ) %>%
    add_markers(
      x = est_matrix[,1],
      y = est_matrix[,2],
      z = est_matrix[,3],
      color = ~inside,
      colors = c("red", "green"),
      marker = list(size = 3),
      name = "Simulations (green = inside)"
    ) %>%
    add_markers(
      x = true_vec[1], y = true_vec[2], z = true_vec[3],
      marker = list(size = 5, color = "blue"),
      name = "True value"
    ) %>%
    layout(
      scene = list(
        xaxis = list(title = "phi"),
        yaxis = list(title = "theta"),
        zaxis = list(title = "var_eta")
      ),
      title = paste0(round(100*(1-alpha)), "% Confidence Ellipsoid with Simulations")
    )

  return(p)
}

power_f = function(base, exponent){
  exponent = pmax(exponent, 0)
  return(base ^ exponent)
}

outer_function_G = function(x, y, ar_coef, ma_coef, vv){
  w = abs(x - y)
  z = vv * (ar_coef + ma_coef + ar_coef * (ar_coef + ma_coef) ^ 2 / (1 - ar_coef ^ 2)) * power_f(ar_coef, w - 1)
  return(as.vector(z))
  #diag_G = var_eta_est * (1 + (phi_est + theta_est) ^ 2 / (1 - phi_est ^ 2))

}

fG = function(size, eta_variance, ar, ma){
  # 1. Compute the exact theoretical autocovariances for ARMA(1,1)
  # Lag 0 variance
  gamma_0 = eta_variance * (1 + (ar + ma)^2 / (1 - ar^2))

  # Lag 1 covariance
  gamma_1 = eta_variance * (ar + ma) * (1 + ar * (ar + ma) / (1 - ar^2))

  # 2. Build the full Toeplitz matrix using lag distances
  lag_matrix <- abs(outer(1:size, 1:size, "-"))

  # 3. Vectorized assignment based on lag steps
  z <- matrix(0, nrow = size, ncol = size)
  z[lag_matrix == 0] <- gamma_0
  z[lag_matrix == 1] <- gamma_1

  # For lags >= 2, the covariance decays geometrically by the AR parameter (phi)
  if (size > 2) {
    z[lag_matrix >= 2] <- gamma_1 * (ar ^ (lag_matrix[lag_matrix >= 2] - 1))
  }

  return(z)
}

equations_arma <- function(vars, ph_hat, theta_b_hat, sigma_a2_hat, sigma_b2_hat) {
  th <- vars[1]
  sigma_eps2 <- vars[2]

  eq1 <- (1 + th^2) * sigma_eps2 + (1 + ph_hat^2) * sigma_a2_hat - (1 + theta_b_hat^2) * sigma_b2_hat
  eq2 <- th * sigma_eps2 - ph_hat * sigma_a2_hat - theta_b_hat * sigma_b2_hat

  return(eq1^2 + eq2^2)
}



plot_conf_ellipsoid_3d <- function(Sigma, mu, alpha = 0.05, ngrid = 40,
                                   plane_range = 3) {
  stopifnot(is.matrix(Sigma), nrow(Sigma) == 3, ncol(Sigma) == 3)
  stopifnot(length(mu) == 3)

  # Chi-square radius
  r2 <- qchisq(1 - alpha, df = 3)

  # Eigen-decomposition
  eig <- eigen(Sigma)
  A <- eig$vectors %*% diag(sqrt(eig$values * r2))

  # Sphere parameterization
  u <- seq(0, 2 * pi, length.out = ngrid)
  v <- seq(0, pi, length.out = ngrid)

  x <- outer(cos(u), sin(v))
  y <- outer(sin(u), sin(v))
  z <- outer(rep(1, length(u)), cos(v))

  # Map unit sphere → ellipsoid
  ellipsoid <- A %*% rbind(as.vector(x),
                           as.vector(y),
                           as.vector(z))

  x_e <- matrix(ellipsoid[1, ] + mu[1], ngrid, ngrid)
  y_e <- matrix(ellipsoid[2, ] + mu[2], ngrid, ngrid)
  z_e <- matrix(ellipsoid[3, ] + mu[3], ngrid, ngrid)

  # Plane: x + y = 0  ⇒  y = -x
  xr <- seq(-plane_range, plane_range, length.out = ngrid)
  zr <- seq(-plane_range, plane_range, length.out = ngrid)

  x_p <- outer(xr, rep(1, ngrid))
  y_p <- -x_p
  z_p <- outer(rep(1, ngrid), zr)

  plot_ly() %>%
    add_surface(
      x = x_e, y = y_e, z = z_e,
      opacity = 0.6,
      name = "Confidence Ellipsoid",
      showscale = FALSE
    ) %>%
    add_surface(
      x = x_p, y = y_p, z = z_p,
      opacity = 0.25,
      surfacecolor = matrix(1, ngrid, ngrid),
      colorscale = list(c(0, "gray"), c(1, "gray")),
      name = "x + y = 0",
      showscale = FALSE
    ) %>%
    add_markers(
      x = mu[1], y = mu[2], z = mu[3],
      marker = list(size = 4),
      name = "Center"
    ) %>%
    layout(
      scene = list(
        xaxis = list(title = "phi"),
        yaxis = list(title = "theta"),
        zaxis = list(title = "sigma2")
      ),
      title = paste0("95% Confidence Ellipsoid with Plane x + y = 0")
    )
}



fwd = function(ar,ma,veta,veps,n){
  threshold = .000001
  if (abs(ar) < threshold && abs(ma) < threshold){

    return(c(0, veta + veps / n))
  }else if(abs(ma / ar - veps / n / veta) < threshold){
    new_ma = 0
    new_var = veta * (ar + ma + ar * (ar + ma) ^ 2 / (1 - ar ^ 2)) * (1 - ar ^ 2) / ar

    return(c(new_ma, new_var))
  }
  else{
    a = ar + ma
    c = 1 - ar ^ 2
    d = veps / veta / n
    e = ar / c - a / c + ar * d / c
    f = 1 + a ^ 2 / c + d
    g = (a + ar * a ^ 2 / c) * (-1)
    b1 = (-f + sqrt(f ^ 2 - 4 * e * g)) / 2 / e
    b2 = (-f - sqrt(f ^ 2 - 4 * e * g)) / 2 / e
    tb1 = b1 - ar
    tb2 = b2 - ar
    s2b = veta * (a + ar * a ^ 2 / c) / (b1 + ar * b1 ^ 2 / c)
    return(c(tb1, s2b))
  }
}

fwd_pois = function(ar,ma,veta,veps,lambda){
  threshold = .000001
  factor = 1 / lambda * (1 - exp(-lambda))
  n = 1 / factor
  if (abs(ar) < threshold && abs(ma) < threshold){

    return(c(0, veta + veps / n))
  }else if(abs(ma / ar - veps / n / veta) < threshold){
    new_ma = 0
    new_var = veta * (ar + ma + ar * (ar + ma) ^ 2 / (1 - ar ^ 2)) * (1 - ar ^ 2) / ar

    return(c(new_ma, new_var))
  }
  else{
    a = ar + ma
    c = 1 - ar ^ 2
    d = veps / veta / n
    e = ar / c - a / c + ar * d / c
    f = 1 + a ^ 2 / c + d
    g = (a + ar * a ^ 2 / c) * (-1)
    b1 = (-f + sqrt(f ^ 2 - 4 * e * g)) / 2 / e
    b2 = (-f - sqrt(f ^ 2 - 4 * e * g)) / 2 / e
    tb1 = b1 - ar
    tb2 = b2 - ar
    s2b = veta * (a + ar * a ^ 2 / c) / (b1 + ar * b1 ^ 2 / c)
    return(c(tb1, s2b))
  }
}

Sigma_o = function(ar, ma_b, veps, v_b, n){
  block11 = (1 + ar * ma_b) / (ar + ma_b) ^ 2 * cbind(c((1 - ar ^ 2) * (1 + ar * ma_b), -(1 - ar ^ 2) * (1 - ma_b ^ 2)), c(-(1 - ar ^ 2) * (1 - ma_b ^ 2), (1 - ma_b ^ 2) * (1 + ar * ma_b)))
  block22 = 2 * v_b ^ 2
  block33 = 2 * veps ^ 2 / n ^ 2 / (n-1)
  full = matrix(0, nrow = 4, ncol = 4)
  full[1:2,1:2] = block11
  full[3,3] = block22
  full[4,4] = block33
  return(full)
}

Sigma_o_varyingn = function(ar, ma_b, veps, v_b, n_t){
  block11 = (1 + ar * ma_b) / (ar + ma_b) ^ 2 * cbind(c((1 - ar ^ 2) * (1 + ar * ma_b), -(1 - ar ^ 2) * (1 - ma_b ^ 2)), c(-(1 - ar ^ 2) * (1 - ma_b ^ 2), (1 - ma_b ^ 2) * (1 + ar * ma_b)))
  block22 = 2 * v_b ^ 2
  n_t = n_t[n_t != 0]
  n_bar = mean(n_t)
  m_hat = mean(1 / n_t)
  S2_n_inverse = var(1 / n_t)
  avar_sigma_a2 = S2_n_inverse * veps ^ 2 + 2 * veps ^ 2 * m_hat ^ 2 / (n_bar - 1)


  block33 = avar_sigma_a2
  full = matrix(0, nrow = 4, ncol = 4)
  full[1:2,1:2] = block11
  full[3,3] = block22
  full[4,4] = block33
  return(full)
}

GG2 = function(ar, ma, veps, veta, n, ma_b, v_b){
  dHdl = diag(rep(1,6))
  dHdl[5,5] = 2 * ma * veta
  dHdl[5,6] = 1 + ma ^ 2
  dHdl[6,5] = veta
  dHdl[6,6] = ma
  dHdl_inv = solve(dHdl)
  dHdo = cbind(c(2 * ar * veps / n, -veps / n),c(-2 * ma_b * v_b, -v_b),c(-(1 + ma_b ^ 2), -ma_b),c(1 + ar ^ 2, -ar))
  dHdo = rbind(diag(rep(-1,4)), dHdo)
  return(-dHdl_inv %*% dHdo)
}

wong = function(ar_hat, ma_b_hat, v_b_hat, v_a_hat, theta_free = TRUE){


  # Parameters
  theta_b_hat = ma_b_hat
  ph_hat = ar_hat
  sigma_a2_hat = v_a_hat
  sigma_b2_hat = v_b_hat

  if (theta_free == FALSE){
    theta_hat = 0
    var_eta_hat = max((1 + theta_b_hat^2) * sigma_b2_hat - (1 + ph_hat ^ 2) * sigma_a2_hat, 0)
    return(c(theta_hat, var_eta_hat))
  }

  # Initial guesses
  aa = theta_b_hat * sigma_b2_hat + ph_hat * sigma_a2_hat
  bb = (1 + ph_hat^2) * sigma_a2_hat - (1 + theta_b_hat ^ 2) * sigma_b2_hat
  cc = aa

  start_ma = 1 / 2 / aa * (-bb - sqrt(bb ^ 2 - 4 * aa * cc))
  start_veta = (theta_b_hat * sigma_b2_hat + ph_hat * sigma_a2_hat) / start_ma
  # --- REPLACE THE RETURN STATEMENTS AT THE BOTTOM OF wong ---
  if (is.na(start_ma) == FALSE && is.na(start_veta) == FALSE){
    return(c(start_ma, start_veta)) # FIX: Drop the 3rd boolean element
  } else {
    # ... BBoptim code ...

      start <- c(theta_b_hat, sigma_b2_hat)


      # Constraints
      lower_bounds <- c(-0.9999, 1e-6)
      upper_bounds <- c(0.9999, Inf)

      # Solve the system
      result <- BB::BBoptim(
        par = start,
        fn = equations_arma,
        ph_hat = ph_hat,
        theta_b_hat = theta_b_hat,
        sigma_a2_hat = sigma_a2_hat,
        sigma_b2_hat = sigma_b2_hat,
        lower = lower_bounds,
        upper = upper_bounds,
        quiet = TRUE,
        control = list(trace = 0)
      )


      theta_hat = result$par[1]
      var_eta_hat = result$par[2]
      return(c(theta_hat, var_eta_hat)) # FIX: Drop the 3rd boolean element
  }
}
#########################
avar = function(phi, theta, var_eps, var_eta, nn, TT, sims){
  var_a = var_eps / nn
  b_vec = fwd(ar = phi, ma = theta, veps = var_eps, veta = var_eta, n = nn)
  theta_b = b_vec[1]
  var_b = b_vec[2]

  S_o = Sigma_o(ar = phi, ma_b = theta_b, veps = var_eps, v_b = var_b, n = nn)
  G = GG(ar = phi, ma = theta, veps = var_eps, veta = var_eta, n = nn, ma_b = theta_b, v_b = var_b)
  S_l = G %*% S_o %*% t(G)

  est_tib = tibble(phi_hat = 0, theta_b_hat = 0, var_b_hat = 0, var_a_hat = 0, theta_hat = 0, var_eta_hat = 0)
  defined_start = 0
  for (sim in 1:sims){
    N = nn * TT
    eps = rnorm(N, mean = 0, sd = sqrt(var_eps))
    u = generate_arma(ar = phi, ma = theta, var = var_eta, length = TT)
    y = rep(u, each = nn) + eps
    t = rep(1:TT, each = nn)
    tib = tibble(day = t, y = y)
    w = tib %>%
      group_by(day) %>%
      summarize(mean = mean(y))
    w = w$mean
    tib$w = rep(w, each = nn)
    tib = tib %>%
      mutate(x = y - w)
    ts_model = arima(w, order = c(1,0,1), include.mean = FALSE)
    phi_hat = ts_model$coef["ar1"]
    theta_b_hat = ts_model$coef["ma1"]
    var_b_hat = ts_model$sigma2
    var_eps_hat = sum(tib$x ^ 2) / TT / (nn - 1)
    var_a_hat = var_eps_hat / nn

    wong_est = wong(ar_hat = phi_hat, ma_b_hat = theta_b_hat, v_b_hat = var_b_hat, v_a_hat = var_eps_hat/nn)


    ###
    var_eta_hat = wong_est[2]
    theta_hat = wong_est[1]
    defined_start[sim] = wong_est[3]
    est_tib = rbind(est_tib, c(phi_hat, theta_b_hat, var_b_hat, var_a_hat, theta_hat, var_eta_hat))
  }
  est_tib = filter(est_tib, phi_hat != 0)
  true_tib = tibble(phi_hat = rep(phi, sims), theta_b_hat = rep(theta_b, sims), var_b_hat = rep(var_b, sims), var_a_hat = rep(var_a, sims), theta_hat = rep(theta, sims), var_eta_hat = rep(var_eta, sims))
  final_tib = sqrt(TT) * (est_tib - true_tib)
  return(list(theta_b, var_b, S_o, est_tib, true_tib, final_tib,  S_l,cov(final_tib), defined_start))
}

theoretical_conf_l = function(phi, theta, var_eta, TT, sims, var_eps, nn, alpha = .05){
  var_a = var_eps / nn
  theta_b = fwd(phi, theta, var_eta, var_eps, nn)[1]
  var_b = fwd(phi, theta, var_eta, var_eps, nn)[2]
  S_o = Sigma_o(phi, theta_b, veps = var_eps, v_b = var_b, n = nn)
  H_0 = dHf_dtau0(phi, theta_b, var_eps / 2, var_b)
  H_1 = dHf_dtau1(theta, var_eta)
  GGG = -solve(H_1) %*% H_0
  S_f = GGG %*% S_o %*% t(GGG)
  S_l = S_f[c(1,5,6), c(1,5,6)]
  chi_q = qchisq(1 - alpha, 3)
  chi_sims = 0
  H_solvable = 0
  for (sim in 1:sims){
    test_u = arima.sim(model = list(ar = phi, ma = theta), n = TT, sd = sqrt(var_eta))
    eps = rnorm(n = TT * nn, mean = 0, sd = sqrt(var_eps))
    y = rep(test_u, each = nn) + eps
    w = t(matrix(y, nrow = nn)) %*% rep(1/nn, nn)
    x = y - rep(w, each = nn)
    var_eps_hat = sum(x ^ 2 / (nn - 1) / TT)
    var_a_hat = var_eps_hat / nn
    arma_noise = arima(w, order = c(1,0,1), include.mean = FALSE)
    phi_hat = arma_noise$coef["ar1"]
    theta_b_hat = arma_noise$coef["ma1"]
    var_b_hat = arma_noise$sigma2
    wong_est = wong(phi_hat, theta_b_hat, var_b_hat, var_eps_hat / nn)
    theta_hat = wong_est[1]
    var_eta_hat = wong_est[2]
    est_vec = c(phi_hat, theta_hat, var_eta_hat)
    true_vec = c(phi, theta, var_eta)
    S_o2 = S_o[1:3,1:3]
    obs_vec = c(phi_hat, theta_b_hat, var_b_hat)
    obs_true = c(phi, theta_b, var_b)
    true_full = c(phi, theta, var_eta, var_eps, theta_b, var_b)
    est_full = c(phi_hat, theta_hat, var_eta_hat, var_eps_hat, theta_b_hat, var_b_hat)
    # print(true_full)
    # print(est_full)
    chi_stat = as.numeric(TT * t(true_vec - est_vec) %*% solve(S_l) %*% (true_vec - est_vec))
    obs_stat = as.numeric(TT * t(obs_true - obs_vec) %*% solve(S_o2) %*% (obs_true - obs_vec))

    chi_sims[sim] = chi_stat < chi_q
    H_solvable[sim] = wong_est[3]
  }
  return(mean(chi_sims))
}



e_fn = function(ar,mab,sigmab2,sigmaa2){
  return(mab * sigmab2 + ar * sigmaa2)
}

f_fn = function(ar,mab,sigmab2,sigmaa2){
  return(-(1 + mab ^ 2) * sigmab2 + (1 + ar ^ 2) * sigmaa2)
}

dF_fn = function(phi,theta_b, sigma_b2, sigma_e2, nn){
  final = matrix(0, nrow = 6, ncol = 4)
  final[1,1] = 1
  final[2,2] = 1
  final[3,3] = 1
  final[4,4] = 1
  wong_est = wong(ar_hat = phi, ma_b_hat = theta_b, v_b_hat = sigma_b2, v_a_hat = sigma_e2/nn)
  ee = e_fn(phi, theta_b, sigma_b2, sigma_a2)
  ff = f_fn(phi, theta_b, sigma_b2, sigma_a2)
  theta = (-ff - sqrt(ff ^ 2 - 4 * ee ^ 2)) / 2 / ee
  var_eta = (theta_b * sigma_b2 + phi * sigma_a2) / theta
  print(theta)
  print(var_eta)
  theta = wong_est[1]
  var_eta = wong_est[2]
  print(theta)
  print(var_eta)

  dedphi = sigma_a2
  dedsa2 = phi
  dedthetab = sigma_b2
  dedsb2 = theta_b
  dfdphi = 2 * phi * sigma_a2
  dfdsa2 = 1 + phi ^ 2
  dfdthetab = -2 * theta_b * sigma_b2
  dfdsb2 = -(1 + theta_b ^ 2)

  final[5,1] = ((-dfdphi - .5 * (ff ^ 2 - 4 * ee ^ 2) ^ (-1 / 2) * (2 * dfdphi * ff - 8 * ee * dedphi)) * 2 * ee + (ff + (ff ^ 2 - 4 * ee ^ 2) ^ (1 / 2)) * 2 * dedphi) / (4 * ee ^ 2)
  final[5,2] = ((-dfdthetab - .5 * (ff ^ 2 - 4 * ee ^ 2) ^ (-1 / 2) * (2 * dfdthetab * ff - 8 * ee * dedthetab)) * 2 * ee + (ff + (ff ^ 2 - 4 * ee ^ 2) ^ (1/2)) * 2 * dedthetab) / (4 * ee ^ 2)
  final[5,3] = ((-dfdsb2 - .5 * (ff ^ 2 - 4 * ee ^ 2) ^ (-1 / 2) * (2 * dfdsb2 * ff - 8 * ee * dedsb2)) * 2 * ee + (ff + (ff ^ 2 - 4 * ee ^ 2) ^ (1/2)) * 2 * dedsb2) / (4 * ee ^ 2)
  final[5,4] = ((-dfdsa2 - .5 * (ff ^ 2 - 4 * ee ^ 2) ^ (-1 / 2) * (2 * dfdsa2 * ff - 8 * ee * dedsa2)) * 2 * ee + (ff + (ff ^ 2 - 4 * ee ^ 2) ^ (1/2)) * 2 * dedsa2) / (4 * ee ^ 2)

  final[6,1] = (sigma_a2 * theta - (theta_b * sigma_b2 + phi * sigma_a2) * final[5,1]) / theta ^ 2
  final[6,2] = (sigma_b2 * theta - (theta_b * sigma_b2 + phi * sigma_a2) * final[5,2]) / theta ^ 2
  final[6,3] = (theta_b * theta - (theta_b * sigma_b2 + phi * sigma_a2) * final[5,3]) / theta ^ 2
  final[6,4] = (phi * theta - (theta_b * sigma_b2 + phi * sigma_a2) * final[5,4]) / theta ^ 2
  return(final)
}

dHf_dtau0 <- function(phi, theta_b, sigma_a2, sigma_b2) {

  M <- matrix(0, nrow = 6, ncol = 4)

  # Identity block (negative)
  diag(M[1:4, 1:4]) <- -1

  # Row 5
  M[5, 1] <-  2 * phi * sigma_a2
  M[5, 2] <- -2 * theta_b * sigma_b2
  M[5, 3] <- -(1 + theta_b^2)
  M[5, 4] <-  (1 + phi^2)

  # Row 6
  M[6, 1] <- -sigma_a2
  M[6, 2] <-  -sigma_b2
  M[6, 3] <- -theta_b
  M[6, 4] <- -phi

  return(M)
}

dHf_dtau1 <- function(theta, sigma_eta2) {

  M <- matrix(0, nrow = 6, ncol = 6)

  # Identity block
  diag(M[1:4, 1:4]) <- 1

  # Row 5
  M[5, 5] <- 2 * theta * sigma_eta2
  M[5, 6] <- 1 + theta^2

  # Row 6
  M[6, 5] <- sigma_eta2
  M[6, 6] <- theta

  return(M)
}

theoretical_conf_o = function(phi, theta, var_eta, TT, sims, var_eps, nn, alpha = .05){
  chi_sims = 0
  theta_b = fwd(phi, theta,var_eta, var_eps, nn)[1]
  var_b = fwd(phi, theta,var_eta, var_eps, nn)[2]
  Sigma_inv = Sigma_o(ar = phi, ma_b = theta_b, veps = var_eps, v_b = var_b, n = nn)[1:3,1:3] %>% solve()
  true_vec = c(phi, theta_b, var_b)
  chi_q = qchisq(p = 1- alpha, df = 3)
  for (sim in 1:sims){
    test_u = arima.sim(model = list(ar = phi, ma = theta), sd = sqrt(var_eta), n = TT)
    eps = rnorm(n = TT * nn, mean = 0, sd = sqrt(var_eps))
    y = rep(test_u, each = nn) + eps
    w = t(matrix(y, nrow = nn)) %*% rep(1/nn, nn)
    x = y - rep(w, each = nn)
    var_eps_hat = sum(x ^ 2) / (nn - 1) / TT
    var_a_hat = var_eps_hat / nn
    arma_noise = arima(w, order = c(1,0,1), include.mean = FALSE)
    phi_hat = arma_noise$coef["ar1"]
    theta_b_hat = arma_noise$coef["ma1"]
    var_b_hat = arma_noise$sigma2
    est_vec = c(phi_hat, theta_b_hat, var_b_hat)
    chi_stat = t(est_vec - true_vec) %*% Sigma_inv %*% (est_vec - true_vec) * TT
    chi_sims[sim] = chi_stat < chi_q
  }
  return(mean(chi_sims))
}

no_th_b_est = function(ph, v_b, v_a){
  a = 1
  b = (1 + ph ^ 2) * v_a - v_b
  c = ph ^ 2 * v_a ^ 2
  sol1 = (-b + sqrt(b ^ 2 - 4 * a * c)) / 2
  sol2 = (-b - sqrt(b ^ 2 - 4 * a * c)) / 2
  th1 = ph * v_a / sol1
  th2 = ph * v_a / sol2
  return(list("veta" = c(sol1, sol2), "theta" = c(th1,th2)))

}



emp_conf_o = function(phi, theta, var_eta, TT, sims, var_eps, nn, alpha = .05){
  chi_sims = 0
  theta_b = fwd(phi, theta,var_eta, var_eps, nn)[1]
  var_b = fwd(phi, theta,var_eta, var_eps, nn)[2]
  true_vec = c(phi, theta_b, var_b)
  chi_q = qchisq(p = 1- alpha, df = 3)
  for (sim in 1:sims){
    test_u = arima.sim(model = list(ar = phi, ma = theta), sd = sqrt(var_eta), n = TT)
    eps = rnorm(n = TT * nn, mean = 0, sd = sqrt(var_eps))
    y = rep(test_u, each = nn) + eps
    w = t(matrix(y, nrow = nn)) %*% rep(1/nn, nn)
    x = y - rep(w, each = nn)
    var_eps_hat = sum(x ^ 2) / (nn - 1) / TT
    var_a_hat = var_eps_hat / nn
    arma_noise = arima(w, order = c(1,0,1), include.mean = FALSE)
    phi_hat = arma_noise$coef["ar1"]
    theta_b_hat = arma_noise$coef["ma1"]
    var_b_hat = arma_noise$sigma2
    est_vec = c(phi_hat, theta_b_hat, var_b_hat)
    Sigma_inv = Sigma_o(ar = phi_hat, ma_b = theta_b_hat, veps = var_eps_hat, v_b = var_b_hat, n = nn)[1:3,1:3] %>% solve()
    chi_stat = t(est_vec - true_vec) %*% Sigma_inv %*% (est_vec - true_vec) * TT
    chi_sims[sim] = chi_stat < chi_q
  }
  return(mean(chi_sims))
}

emp_conf_l = function(phi, theta, var_eta, TT, sims, var_eps, lambda, alpha = .05){

  ll = lambda
  n_t = rpois(n = TT, lambda = lambda) + 1
  arman_pars = fwd_pois(ar = phi, ma = theta, veta = var_eta, veps = var_eps, lambda = ll)

  theta_b = arman_pars[1]
  var_b = arman_pars[2]
  chi_sims = 0
  H_solvable = 0
  for (sim in 1:sims){
    test_u = arima.sim(model = list(ar = phi, ma = theta), n = TT, sd = sqrt(var_eta))
    eps = rnorm(n = sum(n_t), mean = 0, sd = sqrt(var_eps))
    y = rep(test_u, times = n_t) + eps
    df = tibble(y = y, t = rep(1:TT, times = n_t))
    df_w = df |>
      group_by(t) |>
      summarize(w = mean(y))
    w = df_w$w
    x = y - rep(w, times = n_t)
    var_eps_hat = sum(x ^ 2) / (sum(n_t) - TT)
    m_hat = mean(1 / n_t)
    var_a_hat = var_eps_hat * m_hat
    arma_noise = arima(w, order = c(1,0,1), include.mean = FALSE)
    phi_hat = arma_noise$coef["ar1"]
    theta_b_hat = arma_noise$coef["ma1"]
    var_b_hat = arma_noise$sigma2

    wong_est = wong(phi_hat, theta_b_hat, var_b_hat, var_a_hat )
    theta_hat = wong_est[1]
    var_eta_hat = wong_est[2]
    S_o = Sigma_o_varyingn(phi_hat, theta_b_hat, var_eps_hat, var_b_hat, n_t)
    H0 = dHf_dtau0(phi, theta_b, var_a_hat, var_b)
    H1 = dHf_dtau1(theta, var_eta)
    GGG = -solve(H1) %*% H0
    S_f = GGG %*% S_o %*% t(GGG)
    S_l = S_f[c(1,5,6), c(1,5,6)]

    est_vec = c(phi_hat, theta_hat, var_eta_hat)
    true_vec = c(phi, theta, var_eta)
    true_full = c(phi, theta, var_eta, var_eps, theta_b, var_b)
    est_full = c(phi_hat, theta_hat, var_eta_hat, var_eps_hat, theta_b_hat, var_b_hat)
    # print(true_full)
    # print(est_full)

    chi_stat = as.numeric(TT * t(true_vec - est_vec) %*% solve(S_l) %*% (true_vec - est_vec))

    chi_q = qchisq(1 - alpha, 3)
    chi_sims[sim] = chi_stat < chi_q
    H_solvable[sim] = wong_est[3]

  }
  return(mean(chi_sims))
}


make_bar <- function(x0, y0, dx, dy, height, color="blue") {
  # vertices of the cuboid
  verts <- rbind(
    c(x0,      y0,      0),
    c(x0+dx,   y0,      0),
    c(x0+dx,   y0+dy,   0),
    c(x0,      y0+dy,   0),
    c(x0,      y0,      height),
    c(x0+dx,   y0,      height),
    c(x0+dx,   y0+dy,   height),
    c(x0,      y0+dy,   height)
  )

  # faces (triangles)
  i <- c(0,1,2,  0,2,3,   4,5,6,  4,6,7,   0,1,5,  0,5,4,  1,2,6,  1,6,5,
         2,3,7,  2,7,6,   3,0,4,  3,4,7)
  j <- c(1,2,3,  2,3,0,   5,6,7,  6,7,4,   1,5,4,  5,4,0,  2,6,5,  6,5,1,
         3,7,6,  7,6,2,   0,4,7,  4,7,3)
  k <- c(2,3,0,  3,0,1,   6,7,4,  7,4,5,   5,4,0,  4,0,1,  6,5,1,  5,1,2,
         7,6,2,  6,2,3,   4,7,3,  7,3,0)

  list(verts=verts, i=i, j=j, k=k, color=color)
}
plot_ellipsoid = function(name_vec, est_vec, true_vec, Sigma, alpha = .05, df = 3, ngrid = 40){
  est_matrix = matrix(NA, nrow = 1, ncol = 3)
  r2 = qchisq(1 - alpha, df = df)
  eig = eigen(Sigma)
  A = eig$vectors %*% diag(sqrt(eig$values * r2))

  # Sphere parameterization
  u = seq(0, 2*pi, length.out = ngrid)
  v = seq(0, pi, length.out = ngrid)
  x = outer(cos(u), sin(v))
  y = outer(sin(u), sin(v))
  z = outer(rep(1, length(u)), cos(v))

  ellipsoid = A %*% rbind(as.vector(x), as.vector(y), as.vector(z))
  x_e = matrix(ellipsoid[1, ] + est_vec[1], ngrid, ngrid)
  y_e = matrix(ellipsoid[2, ] + est_vec[2], ngrid, ngrid)
  z_e = matrix(ellipsoid[3, ] + est_vec[3], ngrid, ngrid)

  # --- Step 4: generate plot ---
  p <- plot_ly() %>%
    add_surface(
      x = x_e, y = y_e, z = z_e,
      opacity = 0.5,
      name = paste0(round(100*(1-alpha)), "% confidence ellipsoid"),
      showscale = FALSE
    ) %>%
    add_markers(
      x = est_matrix[,1],
      y = est_matrix[,2],
      z = est_matrix[,3],
      #color = ~inside,
      colors = c("red", "green"),
      marker = list(size = 3),
      name = "Simulations (green = inside)"
    ) %>%
    add_markers(
      x = true_vec[1], y = true_vec[2], z = true_vec[3],
      marker = list(size = 5, color = "blue"),
      name = "True value"
    ) %>%
    layout(
      scene = list(
        xaxis = list(title = name_vec[1]),
        yaxis = list(title = name_vec[2]),
        zaxis = list(title = name_vec[3])
      ),
      title = paste0(round(100*(1-alpha)), "% Confidence Ellipsoid with Simulations")
    )

  print(p)
  tmp <- tempfile(fileext = ".html")

  htmlwidgets::saveWidget(p, tmp, selfcontained = TRUE)

  utils::browseURL(tmp)
}
plot_3d_bars <- function(df, xcol="ar", ycol="ma", zcol="conf",
                         dx=NULL, dy=NULL) {

  # rename to standard
  df <- df %>% rename(x = !!xcol, y = !!ycol, z = !!zcol)

  # Determine bar widths (dx, dy) automatically from grid spacing
  if(is.null(dx)){
    dx <- min(diff(sort(unique(df$x))))
  }
  if(is.null(dy)){
    dy <- min(diff(sort(unique(df$y))))
  }

  plt <- plot_ly()

  for (k in seq_len(nrow(df))) {
    bar <- make_bar(df$x[k], df$y[k], dx, dy, df$z[k],
                    color = "steelblue")

    plt <- plt %>% add_mesh(
      x = bar$verts[,1],
      y = bar$verts[,2],
      z = bar$verts[,3],
      i = bar$i,
      j = bar$j,
      k = bar$k,
      opacity = 0.9,
      color = bar$color,
      showscale = FALSE
    )
  }

  plt %>% layout(
    scene = list(
      xaxis = list(title="ar"),
      yaxis = list(title="ma"),
      zaxis = list(title="empirical frequency")
    )
  )
}

sim_arman_reg = function(n_t, beta, X_f, var_eps, ar, ma, veta){
  TT = length(n_t)
  N = sum(n_t)
  u = arima.sim(model = list(ar = ar, ma = ma), sd = sqrt(veta), n = TT)
  eps = rnorm(n = N, sd = sqrt(var_eps), mean = 0)
  X_t = f_Xt(n_t)
  y = as.vector(as.matrix(X_f) %*% beta + X_t %*% u + eps)
  df = tibble(t = as.vector(X_t %*% 1:TT), y = y, u = as.vector(X_t %*% u)) %>% cbind(X_f)
  return(df)
}

f_V = function(n_t){

  TT = length(n_t)
  V_list = list()
  vlistcount = 1
  for (nt in 1:TT){
    nrow = n_t[nt]
    if (nrow != 0){
      c_space = rep(1, nrow)
      null_basis = Null((c_space))
      ortho_basis <- qr.Q(qr(null_basis))
      V_list[[vlistcount]] = ortho_basis
      vlistcount = vlistcount + 1
    }

  }
  V = bdiag(V_list)
  VtV_list = Map(function(a, b) a %*% t(b), V_list, V_list)
  VtV = bdiag(VtV_list)
  VtV = Matrix(VtV, sparse = TRUE)
  V = Matrix(V, sparse = TRUE)
  return(V)
}


f_u_hat_fixedn = function(n, T, residuals, Gamma, var_epsilon){
  X_t = f_Xt(rep(n,T))
  V = f_V(rep(n,T))

  G_eigen = eigen(Gamma)
  G_evec = G_eigen$vectors
  G_eval = G_eigen$values
  X_t = Matrix(X_t, sparse = TRUE)
  L = diag(G_eval) %>% Matrix(sparse = TRUE)
  D_inv = diag(1 / (n * G_eval + var_epsilon))
  D_inv = Matrix(D_inv, sparse = TRUE)
  # VVt = V %*% t(V) / var_eps_hat
  u1 = G_evec %*% L %*% D_inv %*% t(G_evec) %*% t(X_t) %*% residuals
  # u2 = G_hat %*% t(X_t) %*% VVt %*% residuals(me) / var_eps_hat

  return(u1)
}

f_u_hat = function(n_t, residuals, Gamma, var_epsilon){
  TT = length(n_t)
  N = sum(n_t)
  X_t = f_Xt(n_t)
  sqN = diag(sqrt(n_t))
  NGN_eigen = eigen(sqN %*% Gamma %*% sqN)

  NGN_evec = NGN_eigen$vectors
  NGN_eval = NGN_eigen$values
  X_t = Matrix(X_t, sparse = TRUE)
  L = diag(NGN_eval + var_eps_hat) %>% Matrix(sparse = TRUE)

  # VVt = V %*% t(V) / var_eps_hat
  u1 = Gamma %*% sqN %*% NGN_evec %*% solve(L) %*%  t(NGN_evec) %*% sqN
  u1 = u1[n_t != 0, n_t != 0]
  u1 = u1 %*%  residuals[n_t != 0]
  # u2 = G_hat %*% t(X_t) %*% VVt %*% residuals(me) / var_eps_hat

  return(u1 %>% as.vector())
}
arman_gls_em = function(sims, TT, ar, ma, veta, veps, beta, alpha = .05, lambda_n = 20){

  ff_initial = "y ~ 1 + t + x"
  ff_update = "less_u_hat ~ 1 + t + x"

  xi2captured_track_gls = c()

  chi_beta_track1_gls = c()
  chi_beta_track2_gls = c()
  chi_beta_track3_gls = c()

  for (sim in 1:sims){
    sim_start = Sys.time()
    n_t = rpois(n = TT, lambda = lambda_n) + 1
    X_t = f_Xt_sparse(n_t)
    N = diag(n_t)


    var_eps = veps
    var_eta = veta
    phi = ar
    theta = ma

    fwd_est = fwd(phi, theta, var_eta, var_eps, (mean(n_t[n_t != 0]^(-1)))^(-1))
    theta_b = fwd_est[1]
    var_b = fwd_est[2]
    var_a = var_eps * mean(n_t[n_t != 0]^(-1))

    X_f = tibble(intercept = 1, day = as.vector(X_t %*% (1:TT)), x = rbinom(n = sum(n_t), size = 1, prob = .5))
    X_f_m = as.matrix(X_f)


    test = sim_arman_reg(n_t,
                         beta,
                         X_f ,
                         var_eps ,
                         ar ,
                         ma ,
                         veta )



    lm_initial = lm(test, formula = ff_initial)


    rmarcs = RCSfixed.fit(mod = lm_initial, dat = test, resp_var = "y")
    phi_hat = rmarcs$TS_Pars[1,1]
    theta_hat = rmarcs$TS_Pars[2,1]
    var_eta_hat = rmarcs$`TS Variance`
    S_l_plot = rmarcs$`TS Covariance`



    vec_est = c(phi_hat, theta_hat, var_eta_hat)
    vec_true = c(phi, theta, var_eta)
    vec_name = c("phi", "theta", "var_eta")

    #plot_ellipsoid(name_vec = vec_name, est_vec = vec_est, true_vec = vec_true, Sigma = S_l_plot)
    cutoff = qchisq(p = .95, df = 3)
    chistat = t(vec_est - vec_true) %*% solve(S_l_plot) %*% (vec_est - vec_true)

    xi2chi2 = chistat |> as.numeric()
    xi2captured_track_gls = xi2captured_track_gls %>% append(xi2chi2)

    beta_cutoff = qchisq(p = 1 - alpha, df = length(beta))



    beta_hat = rmarcs$Beta_Pars[,1]


    var_beta = rmarcs$`Fixed Covariance`





    chistat_beta3 = t(beta_hat - beta) %*% solve(var_beta) %*% (beta_hat - beta)

    chistat_beta3 = chistat_beta3 %>% as.numeric()

    chi_beta_track3_gls = chi_beta_track3_gls %>% append(chistat_beta3)


    sim_end = Sys.time()
    #print(sim_end - sim_start)

  }

  # xi1 = mean(chi_beta_track3_gls < beta_cutoff)
  # xi2 = mean(xi2captured_track_gls < cutoff)
print("done")

  return(list(l1_conf = mean(chi_beta_track3_gls < cutoff),l2_conf = mean(xi2captured_track_gls < cutoff)))
}
emp_exp_ar_vs_arma = function(phi, v_a, TT, sims){
  phi_mean_ar = c()
  phi_mean_arma = c()
  veta_mean_ar = c()
  veta_mean_arma = c()
  for (sim in 1:sims){
    uu = arima.sim(model = list(ar = phi), sd = 1, n = TT)
    aa = rnorm(n = TT, mean = 0, sd = sqrt(v_a))
    ww = uu + aa
    ar_model = arima(ww,
                     order = c(1,0,0),
                     include.mean = FALSE,
                     method = "ML"
                     )
    arma_model = arima(ww, order = c(1,0,1), include.mean = FALSE,
                       method = "ML")
    phi_hat_ar = ar_model$coef["ar1"]
    phi_hat_arma = arma_model$coef["ar1"]
    veta_hat_ar = ar_model$sigma2
    veta_hat_arma = arma_model$sigma2
    phi_mean_ar = append(phi_mean_ar, phi_hat_ar)
    phi_mean_arma = append(phi_mean_arma, phi_hat_arma)
    veta_mean_ar = append(veta_mean_ar, veta_hat_ar)
    veta_mean_arma = append(veta_mean_arma, veta_hat_arma)
  }
  return(c(phi_mean_ar = phi_mean_ar %>% mean(),
           phi_mean_arma = phi_mean_arma %>% mean(),
           veta_mean_ar = veta_mean_ar %>% mean(),
           veta_mean_arma = veta_mean_arma %>% mean()))
}

G_theta = function(ar, ma, veta, ma_b, v_b, veps, m_hat){
  dH_dtau0 = matrix(0, nrow = 3, ncol = 4)
  dH_dtau0[1,4] = -1
  dH_dtau0[2,1] = -1
  dH_dtau0[3,1] = -2 * ar * veps * m_hat
  dH_dtau0[3,2] = 2 * ma_b * v_b
  dH_dtau0[3,3] = 1 + ma_b^2
  dH_dtau0[3,4] = 1 + ar^2 * m_hat
  dH_dtau1 = diag(c(1,1,-1))
  G = -solve(dH_dtau1) %*% dH_dtau0
  return(G)
}

are = function(df, ff, tol = .1){
  TT = max(df$t)
  linmod = lm(data = df, formula = ff)
  l_old = logLik(linmod)



  df$resid = residuals(linmod)
  df_t = df |>
    group_by(t) |>
    summarize(mean_resid = mean(resid), n = n())
  df_t = left_join(
    tibble(t = 1:max(df_t$t)),
    df_t
  )
  u_hat = df_t$mean_resid
  arma = arima(u_hat, order = c(1,0,0))
  phi_hat = arma$coef["ar1"]
  var_eta_hat = arma$sigma2
  G_hat = fG(size = TT,
             eta_variance = var_eta_hat,
             ar = phi_hat,
             ma = 0)

  G_inv = G_hat |>
    chol() |>
    chol2inv()
  lt = -TT/2 * log(2 * pi) - TT / 2 * log(var_eta_hat) + 1 / 2 * log(1 - phi_hat ^ 2) - 1 / 2 * t(u_hat) %*% G_inv %*% u_hat

  df$no_u_hat = df$y - as.vector(X_t %*% u_hat)
  form = formula(linmod)
  form[[2]] = quote(no_u_hat)
  lm_new = lm(formula = form, data = df)
  var_eps_hat = sigma(lm_new) ^ 2
  l1_new = logLik(lm_new)
  l_new = lt + l1_new
  X_t = f_Xt_sparse(n_vec = df_t$n)
  while(abs(l_old - l_new) > tol){
    l_old = l_new
    G_hat = fG(size = TT,
               eta_variance = var_eta_hat,
               ar = phi_hat,
               ma = 0)

    G_inv = G_hat |>
      chol() |>
      chol2inv()
    Gplus_inv = G_inv + Matrix::t(X_t) %*% X_t / var_eps_hat
    Gplus_inv = Gplus_inv |>
      chol() |>
      chol2inv()
    Oinvresid = (df$resid / var_eps_hat) - var_eps_hat ^ (-2) * (X_t %*% (Gplus_inv %*% (Matrix::t(X_t) %*% df$resid)))
    u_hat = (G_hat %*% (Matrix::t(X_t) %*% Oinvresid)) %>% as.vector()
    arma = arima(u_hat, order = c(1,0,0))
    phi_hat = arma$coef["ar1"]
    var_eta_hat = arma$sigma2

    lt = -TT/2 * log(2 * pi) - TT / 2 * log(var_eta_hat) + 1 / 2 * log(1 - phi_hat ^ 2) - 1 / 2 * t(u_hat) %*% G_inv %*% u_hat
    df$no_u_hat = df$y - as.vector(X_t %*% u_hat)
    lm_new = lm(formula = form, data = df)
    var_eps_hat = sigma(lm_new) ^ 2
    l1_new = logLik(lm_new)
    l_new = lt + l1_new
  }
  summ = summary(lm_new)
  summ$sigma
  ret_list = list(beta = lm_new$coefficients,
                  veps = summ$sigma ^ 2,
                  phi = phi_hat,
                  veta = var_eta_hat)
}


estimate_beta_gls <- function(
    y,
    X_f,
    X_r,
    X_t,
    Sigma_b,
    Gplus_inv,
    sigma_eps2
) {

  # ---------------------------------------------------------
  # 1. Constants
  # ---------------------------------------------------------

  c <- 1 / sigma_eps2


  # ---------------------------------------------------------
  # 2. Cross-products involving X_t
  # ---------------------------------------------------------

  XtXr <- Matrix::crossprod(X_t, X_r)  # X_t' X_r
  XtXf <- Matrix::crossprod(X_t, X_f)  # X_t' X_f
  Xty  <- Matrix::crossprod(X_t, y)    # X_t' y


  # ---------------------------------------------------------
  # 3. K = X_r' A^{-1} X_r
  # ---------------------------------------------------------

  XrXr <- Matrix::crossprod(X_r)

  K <- c * XrXr -
    c^2 * Matrix::crossprod(
      XtXr,
      Gplus_inv %*% XtXr
    )


  # ---------------------------------------------------------
  # 4. B = Sigma_b^{-1} + K
  # ---------------------------------------------------------

  Sigma_b_inv <- Matrix::solve(Sigma_b)

  B <- Sigma_b_inv + K


  # ---------------------------------------------------------
  # 5. A^{-1} X_f
  # ---------------------------------------------------------

  Ainv_Xf <- c * X_f -
    c^2 * X_t %*% (
      Gplus_inv %*% XtXf
    )


  # ---------------------------------------------------------
  # 6. A^{-1} y
  # ---------------------------------------------------------

  Ainv_y <- c * y -
    c^2 * X_t %*% (
      Gplus_inv %*% Xty
    )


  # ---------------------------------------------------------
  # 7. X_r' A^{-1} X_f
  # ---------------------------------------------------------

  Xr_Ainv_Xf <- Matrix::crossprod(
    X_r,
    Ainv_Xf
  )


  # ---------------------------------------------------------
  # 8. X_r' A^{-1} y
  # ---------------------------------------------------------

  Xr_Ainv_y <- Matrix::crossprod(
    X_r,
    Ainv_y
  )


  # ---------------------------------------------------------
  # 9. Compute:
  #
  # X_f' Sigma_y^{-1} X_f
  #
  # = X_f' A^{-1} X_f
  #   - X_f' A^{-1} X_r
  #     B^{-1}
  #     X_r' A^{-1} X_f
  # ---------------------------------------------------------

  Xf_Ainv_Xf <- Matrix::crossprod(
    X_f,
    Ainv_Xf
  )

  correction_Xf <- Matrix::t(Xr_Ainv_Xf) %*%
    Matrix::solve(B, Xr_Ainv_Xf)

  Xt_Siginv_Xf <- Xf_Ainv_Xf -
    correction_Xf


  # ---------------------------------------------------------
  # 10. Compute:
  #
  # X_f' Sigma_y^{-1} y
  #
  # = X_f' A^{-1} y
  #   - X_f' A^{-1} X_r
  #     B^{-1}
  #     X_r' A^{-1} y
  # ---------------------------------------------------------

  Xf_Ainv_y <- Matrix::crossprod(
    X_f,
    Ainv_y
  )

  correction_y <- Matrix::t(Xr_Ainv_Xf) %*%
    Matrix::solve(B, Xr_Ainv_y)

  Xf_Siginv_y <- Xf_Ainv_y -
    correction_y


  # ---------------------------------------------------------
  # 11. GLS estimate
  # ---------------------------------------------------------

  beta_hat <- Matrix::solve(
    Xt_Siginv_Xf,
    Xf_Siginv_y
  )


  # ---------------------------------------------------------
  # 12. Return useful quantities
  # ---------------------------------------------------------

  return(list(
    beta_hat = beta_hat,
    B = B,
    Xt_Siginv_Xf = Xt_Siginv_Xf,
    Xf_Siginv_y = Xf_Siginv_y
  ))
}

are_stability = function(sims,
                         TT,
                         ar,
                         ma,
                         veta,
                         veps,
                         beta,
                         alpha = .05,
                         lambda_n = 20,
                         arma_order = c(1,0,0)){

  ff_initial = "y ~ 1 + t + x"
  ff_update = "less_u_hat ~ 1 + t + x"

  phi_track = c()
  veta_track = c()
  phi_captured = c()


  for (sim in 1:sims){
    sim_start = Sys.time()
    n_t = rpois(n = TT, lambda = lambda_n) + 1
    X_t = f_Xt_sparse(n_t)
    #N = diag(n_t)


    var_eps = veps
    var_eta = veta
    phi = ar
    theta = ma

    fwd_est = fwd(phi, theta, var_eta, var_eps, (mean(n_t[n_t != 0]^(-1)))^(-1))
    theta_b = fwd_est[1]
    var_b = fwd_est[2]
    var_a = var_eps * mean(n_t[n_t != 0]^(-1))

    X_f = tibble(intercept = 1, day = as.vector(X_t %*% (1:TT)), x = rbinom(n = sum(n_t), size = 1, prob = .5))
    X_f_m = as.matrix(X_f)


    test = sim_arman_reg(n_t,
                         beta,
                         X_f ,
                         var_eps ,
                         ar ,
                         ma ,
                         veta )
    test$resid = as.vector(test$y - X_f_m %*% beta)

    G_hat = fG(size = TT, eta_variance = veta, ar = ar, ma = ma)



    G_inv = G_hat |>
      chol() |>
      chol2inv()
    Gplus_inv = G_inv + Matrix::t(X_t) %*% X_t / veps
    Gplus_inv = Gplus_inv |>
      chol() |>
      chol2inv()
    Oinvresid = (test$resid / veps) - veps ^ (-2) * (X_t %*% (Gplus_inv %*% (Matrix::t(X_t) %*% test$resid)))
    u_hat = (G_hat %*% (Matrix::t(X_t) %*% Oinvresid)) %>% as.vector()
    arma_hat = arima(u_hat, order = arma_order)
    phi_hat = arma_hat$coef["ar1"]
    var_eta_hat = arma_hat$sigma2
    phi_track = phi_track |> append(phi_hat)
    veta_track = veta_track |> append(var_eta_hat)
    phi_sd = arma_hat$var.coef[1,1] |> sqrt()
    zscore = abs(qnorm(p = alpha / 2))
    phi_capt = phi < phi_hat + phi_sd * zscore & phi > phi_hat - phi_sd * zscore
    phi_captured = phi_captured |> append(phi_capt)
  }

  # xi1 = mean(chi_beta_track3_gls < beta_cutoff)
  # xi2 = mean(xi2captured_track_gls < cutoff)
  return(list(phi = mean(phi_track), phi_capt = mean(phi_captured), var_eta = mean(veta_track)))
}

arman_gls_ms = function(sims, TT, ar, ma, veta, veps, beta, alpha = .05, lambda_n = 20){
  ff_initial = "y ~ 1 + t + x"
  ff_update = "less_u_hat ~ 1 + t + x"

  theta_track = logical(sims)
  phi_track = logical(sims)
  theta_b_track = logical(sims)
  theta_track = logical(sims)
  var_eta_track = logical(sims)
  var_eps_track = logical(sims)
  int_track = logical(sims)
  slope_track = logical(sims)
  cov_track = logical(sims)


  for (sim in 1:sims){
    sim_start = Sys.time()
    n_t = rpois(n = TT, lambda = lambda_n) + 1
    X_t = f_Xt_sparse(n_t)
    N = diag(n_t)


    var_eps = veps
    var_eta = veta
    phi = ar
    theta = ma

    fwd_est = fwd(phi, theta, var_eta, var_eps, (mean(n_t[n_t != 0]^(-1)))^(-1))
    theta_b = fwd_est[1]
    var_b = fwd_est[2]
    var_a = var_eps * mean(n_t[n_t != 0]^(-1))

    X_f = tibble(intercept = 1, day = as.vector(X_t %*% (1:TT)), x = rbinom(n = sum(n_t), size = 1, prob = .5))
    X_f_m = as.matrix(X_f)


    test = sim_arman_reg(n_t,
                         beta,
                         X_f ,
                         var_eps ,
                         ar ,
                         ma ,
                         veta )



    lm_initial = lm(test, formula = ff_initial)


    rmarcs = RCSfixed.fit(mod = lm_initial, dat = test, resp_var = "y")
    phi_hat = rmarcs$TS_Pars[1,1]
    theta_hat = rmarcs$TS_Pars[2,1]
    var_eta_hat = rmarcs$`TS Variance`
    S_l_plot = rmarcs$`TS Covariance`



    vec_est = c(phi_hat, theta_hat, var_eta_hat)
    vec_true = c(phi, theta, var_eta)
    vec_name = c("phi", "theta", "var_eta")

    #plot_ellipsoid(name_vec = vec_name, est_vec = vec_est, true_vec = vec_true, Sigma = S_l_plot)



    zscore = qnorm(p = alpha / 2) |> abs()
    sd_theta <- sqrt(S_l_plot[2,2])
    theta_capt = ma > theta_hat - zscore * sd_theta & ma < theta_hat + zscore * sd_theta
    sd_phi <- sqrt(S_l_plot[1,1])
    phi_capt = (ar > (phi_hat - zscore * sd_phi)) & (ar < (phi_hat + zscore * sd_phi))

    theta_track[sim] <-(theta_capt)
    phi_track[sim] <- (phi_capt)
  }

  # xi1 = mean(chi_beta_track3_gls < beta_cutoff)
  # xi2 = mean(xi2captured_track_gls < cutoff)
  return(list(#theta_insig = mean(theta_insig),

              theta_bar = mean(theta_track),
              # thetab_bar = mean(theta_b_track),
              # veta_bar = mean(var_eta_track),
              # veps_bar = mean(var_eps_track),
              # int_bar = mean(int_track),
              # slope_bar = mean(slope_track),
              # cov_bar = mean(cov_track),
              phi_bar = mean(phi_track)
              ))
  }

