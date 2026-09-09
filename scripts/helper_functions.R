
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
    cutoff = qchisq(p = .95, df = 2)
    chistat = t(vec_est[1:2] - vec_true[1:2]) %*% solve(S_l_plot[1:2,1:2]) %*% (vec_est[1:2] - vec_true[1:2])

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
#print("done")

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



    # ... [previous code] ...

    G_inv = G_hat |> chol() |> chol2inv()
    Gplus_inv = G_inv + Matrix::t(X_t) %*% X_t / veps

    # SAFE INVERSION OF Gplus
    Gplus_inv_calculated <- tryCatch({
      Gplus_inv |> chol() |> chol2inv()
    }, error = function(e) {
      cat("Error in Gplus_inv calculation at iteration:", sim, "\n")
      return(NULL) # Flag a failure
    })

    if (is.null(Gplus_inv_calculated)) next # Skip this bad simulation run
    Gplus_inv <- Gplus_inv_calculated

    Oinvresid = (test$resid / veps) - veps ^ (-2) * (X_t %*% (Gplus_inv %*% (Matrix::t(X_t) %*% test$resid)))
    u_hat = (G_hat %*% (Matrix::t(X_t) %*% Oinvresid)) %>% as.vector()

    # SAFE ARIMA ESTIMATION
    arma_hat <- tryCatch({
      arima(u_hat, order = arma_order)
    }, error = function(e) {
      cat("Error in arima optimization at iteration:", sim, "\n")
      return(NULL)
    })

    if (is.null(arma_hat)) next # Skip if arima matrix inversion failed

    phi_hat = arma_hat$coef["ar1"]
    # ... [rest of loop continues safely] ...

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
  return(list(phi = mean(phi_track, na.rm = TRUE),
              phi_capt = mean(phi_captured, na.rm = TRUE),
              var_eta = mean(veta_track, na.rm = TRUE),
              sims = length(na.exclude(phi_track))
              )
         )
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
RCSfixed.fit <- function(mod, dat, resp_var, tol = 0.1, max_updates = 10)
{
  y = dat[[resp_var]]
  resid = residuals(mod)
  dat$resid = resid

  df_w = dat |>
    group_by(t) |>
    summarize(w = mean(resid), S2 = var(resid), n = n())



  df_w = left_join(tibble(t = 1:max(df_w$t)), df_w, by = "t")
  # acf(df_w$w, na.action = na.pass, main = "ACF of average residuals")

  #Generate initial ARMA(1,1) estimates
  arma_w = arima(df_w$w, order = c(1,0,1), include.mean = FALSE, method = "ML")
  phi_hat = arma_w$coef["ar1"]
  theta_b_hat = arma_w$coef["ma1"]
  var_b_hat = arma_w$sigma2


  df_w$n = replace_na(df_w$n, 0)
  df_w_initial = df_w
  T_len = nrow(df_w)

  X_f = model.matrix(mod)

  X_f_m = X_f

  l1 = logLik(mod) |> as.numeric()
  dat$resid_initial = dat$resid

  #Get initial estimates
  n_t = df_w$n
  X_t = f_Xt_sparse(n_t)
  V = f_V(n_t)
  # T = length(n_t)
  N = diag(n_t)
  df_w_veps_est = df_w |>
    filter(n > 1)
  var_eps_hat = sum((df_w_veps_est$n - 1) * df_w_veps_est$S2) / (sum(df_w_veps_est$n) - length(df_w_veps_est$n))
  var_a_hat = var_eps_hat * mean((n_t[n_t != 0])^{-1})
  wong_est = wong(ar_hat=phi_hat, ma_b_hat=theta_b_hat,v_b_hat=var_b_hat,v_a_hat=var_a_hat)
  theta_hat = wong_est[1]
  var_eta_hat = wong_est[2]
  G_hat = fG(size = T_len, eta_variance = var_eta_hat, ar = phi_hat, ma = theta_hat)
  G_inv = G_hat |>
    chol() |>
    chol2inv()
  Gplus_inv = G_inv + Matrix::t(X_t) %*% X_t / var_eps_hat
  Gplus_inv = Gplus_inv |>
    chol() |>
    chol2inv()
  Oinvresid = (dat$resid / var_eps_hat) - var_eps_hat ^ (-2) * (X_t %*% (Gplus_inv %*% (Matrix::t(X_t) %*% dat$resid)))
  u_hat = (G_hat %*% (Matrix::t(X_t) %*% Oinvresid)) %>% as.vector()
  l2 = -T_len/2 * log(2 * pi) - T_len / 2 * log(var_eta_hat) + 1 / 2 * log(1 - phi_hat ^ 2) - 1 / 2 * t(u_hat) %*% G_inv %*% u_hat
  l_new = l2 + l1
  l_old = l_new + 2 * tol
  l1_track = l1
  l2_track = l2
  l_track = l_old
  phi_track = phi_hat
  theta_track = theta_hat
  var_eta_track = var_eta_hat
  var_eps_track = var_eps_hat
  updates = 1
  while (abs(l_old-l_new) > tol & updates <= max_updates){
    updates = updates + 1
    l_old = l_new
    dat$less_u_hat = dat[[resp_var]] - as.vector(X_t[,n_t != 0] %*% u_hat)
    lm_new = update(mod,less_u_hat~.,data=dat)
    l1 = lm_new |> logLik() |> as.numeric() #This is the likelihood of the fitted model after subtracting the time series
    Oinvy = (dat[[resp_var]] / var_eps_hat) - var_eps_hat ^ (-2) * (X_t %*% (Gplus_inv %*% (Matrix::t(X_t) %*% dat[[resp_var]])))
    OinvXf = (X_f_m / var_eps_hat) - var_eps_hat ^ (-2) * (X_t %*% (Gplus_inv %*% (Matrix::t(X_t) %*% X_f_m)))
    beta_hat = solve(Matrix::t(X_f_m) %*% OinvXf) %*% Matrix::t(X_f_m) %*% Oinvy
    beta_hat = as.numeric(beta_hat)
    dat$resid = as.vector(dat[[resp_var]]- X_f_m %*% beta_hat)

    df_w = dat |>
      group_by(t) |>
      summarize(w = mean(resid), S2 = var(resid), n = n())

    df_w = left_join(tibble(t = 1:max(df_w$t)), df_w, by = "t")
    df_w$n = replace_na(df_w$n, 0)

    arma_w = arima(df_w$w, order = c(1,0,1), include.mean = FALSE, method = "ML")
    phi_hat = arma_w$coef["ar1"]
    theta_b_hat = arma_w$coef["ma1"]
    var_b_hat = arma_w$sigma2


    df_w_veps_est = df_w |>
      filter(n > 1)

    var_eps_hat = sum((df_w_veps_est$n - 1) * df_w_veps_est$S2) / (sum(df_w_veps_est$n) - length(df_w_veps_est$n))
    var_a_hat = var_eps_hat * mean((n_t[n_t != 0])^{-1})

    wong_est = wong(phi_hat, theta_b_hat, var_b_hat, var_a_hat)
    theta_hat = wong_est[1]
    var_eta_hat = wong_est[2]
    G_hat = fG(size = T_len, eta_variance = var_eta_hat, ar = phi_hat, ma = theta_hat)

    G_inv = G_hat |>
      chol() |>
      chol2inv()
    Gplus_inv = G_inv + Matrix::t(X_t) %*% X_t / var_eps_hat
    Gplus_inv = Gplus_inv |>
      chol() |>
      chol2inv()
    Oinvresid = (dat$resid / var_eps_hat) - var_eps_hat ^ (-2) * (X_t %*% (Gplus_inv %*% (Matrix::t(X_t) %*% dat$resid)))
    u_hat = (G_hat %*% (Matrix::t(X_t) %*% Oinvresid)) %>% as.vector()

    l2 = -T_len/2 * log(2 * pi) - T_len / 2 * log(var_eta_hat) + 1 / 2 * log(1 - phi_hat ^ 2) - 1 / 2 * t(u_hat) %*% G_inv %*% u_hat


    l_new = l2 + l1
    l1_track = l1_track |> append(l1)
    l2_track = l2_track |> append(l2)
    l_track = l_track |> append(l_new) #overall likelihood
    # cat("likelihood sequence for regression model: \n")
    #
    # cat("likelihood sequence for time series model: \n")

    phi_track = phi_track |> append(phi_hat)
    theta_track = theta_track |> append(theta_hat)
    var_eta_track = var_eta_track |> append(var_eta_hat)
    var_eps_track = var_eps_track |> append(var_eps_hat)
  }

  S_o = Sigma_o_varyingn(phi_hat, theta_b_hat, var_eps_hat, var_b_hat, n_t)

  print(updates)

  H1 = dHf_dtau1(theta_hat, var_eta_hat)
  H0 = dHf_dtau0(phi_hat, theta_b_hat, var_a_hat, var_b_hat)
  GGG = -solve(H1) %*% H0
  S_f = GGG %*% S_o %*% t(GGG)
  S_l = S_f[c(1,5,6),c(1,5,6)]
  S_l_plot = S_l / T_len
  S_l_plot = as.matrix(S_l_plot)
  vec_est = c(phi_hat, theta_hat, var_eta_hat)
  vec_true = c(phi_hat, theta_hat, var_eta_hat)
  vec_name = c("phi", "theta", "var_eta")
  ts.pars <- c(
    phi_hat,
    theta_hat
  )

  names(ts.pars) <- c(
    "phi",
    "theta"
  )

  ts.pars <- cbind(
    ts.pars,
    sqrt(diag(S_l_plot)[1:2])
  )

  pvals_ts <- 2 * (
    1 - stats::pnorm(
      abs(ts.pars[, 1] / ts.pars[, 2])
    )
  )

  ts.pars <- cbind(
    ts.pars,
    pvals_ts
  )
  OinvXf = (X_f_m / var_eps_hat) - var_eps_hat ^ (-2) * (X_t %*% (Gplus_inv %*% (Matrix::t(X_t) %*% X_f_m)))
  var_bet = solve(Matrix::t(X_f_m) %*% OinvXf)
  var_bet = as.matrix(var_bet)
  beta_hat = cbind(beta_hat, sqrt(diag(var_bet)))
  pvals=2*(1-pnorm(abs(beta_hat[,1]/beta_hat[,2])))
  beta_hat=cbind(beta_hat,pvals)
  # --- REPLACE THE BOTTOM RETURN BLOCK WITH THIS ---
  AIC = 2 * (nrow(beta_hat) + 3) - 2 * l_new

  # FIX: Use the dense matrix X_f_m instead of the tibble X_f, and drop S4 Matrix tracking
  reg_fitted = as.numeric(X_f_m %*% beta_hat[, 1])
  ts_fitted  = as.numeric(X_t %*% u_hat)

  fitted.mat = data.frame(
    Day = dat$t,
    Reg = reg_fitted,
    TS  = ts_fitted
  )

  res.list = list(
    beta_hat,
    ts.pars,
    l_new,
    AIC,
    fitted.mat,
    var_eps_hat,
    var_eta_hat,
    var_bet,
    S_l_plot
  )

  names(res.list) = c("Beta_Pars", "TS_Pars", "log(likelihood)", "AIC", "Fitted_Values",
                      "Individual Variance", "TS Variance", "Fixed Covariance", "TS Covariance")
  return(res.list)

}

RCSmixed.fit <- function(mod, dat, resp_var, tol = 0.1, max_updates = 10, arma_order = c(1,1)) {

  # --------------------------------------------------------------------------
  # Initial mixed-effects model and daily residual summaries
  # --------------------------------------------------------------------------

  y = dat[[resp_var]]
  resid <- stats::residuals(mod)
  dat$resid <- resid
  var_re = sigma(mod)^2 * tcrossprod(getME(mod, "Lambda"))

  # --------------------------------------------------------------------------
  # Aggregate residuals
  # --------------------------------------------------------------------------

  df_w <- dat |>
    dplyr::group_by(t) |>
    dplyr::summarise(
      w = mean(resid),
      S2 = stats::var(resid),
      n = dplyr::n(),
      .groups = "drop"
    )
  n_t = df_w$n


  # Fill in missing days so that the time-series component has a complete
  # sequence of time points.

  df_w <- dplyr::left_join(
    tibble::tibble(t = 1:max(df_w$t)),
    df_w,
    by = "t"
  )

  # --------------------------------------------------------------------------
  # Initial ARMA(1, 1) estimates
  # --------------------------------------------------------------------------

  arma_w <- stats::arima(
    df_w$w,
    order = c(1, 0, 1),
    include.mean = FALSE
  )

  print(acf(df_w$w, na.action = na.pass))
  print(arma_w)
  # S_o = Sigma_o_varyingn(ar = phi_hat, ma_b = theta_b_hat, veps = var_eps_hat, v_b = var_b_hat, n_t = n_t)
  # conf_region_2d(theta_hat = c(phi_hat, theta_b_hat),
  #                theta_true = c(phi_hat, theta_b_hat),
  #                Sigma = S_o[1:2,1:2] / T) |> print()


  phi_hat <- arma_w$coef["ar1"]
  theta_b_hat <- arma_w$coef["ma1"]
  var_b_hat <- arma_w$sigma2





  df_w$n <- tidyr::replace_na(df_w$n, 0)
  df_w_initial <- df_w

  # --------------------------------------------------------------------------
  # Design matrices and initial variance estiamtes
  # --------------------------------------------------------------------------

  X_f <- stats::model.matrix(mod)
  X_f_m <- X_f

  X_r <- lme4::getME(mod, "Z")
  vc <- lme4::VarCorr(mod)

  var_re <- stats::sigma(mod)^2 *
    Matrix::tcrossprod(lme4::getME(mod, "Lambda"))

  l1 <- as.numeric(stats::logLik(mod))
  dat$resid_initial <- dat$resid

  n_t <- df_w$n
  X_t <- f_Xt(n_t)
  V <- f_V(n_t)
  T <- length(n_t)
  N <- diag(n_t)

  df_w_veps_est <- df_w |>
    dplyr::filter(n > 1)

  var_eps_hat <- sum(
    (df_w_veps_est$n - 1) * df_w_veps_est$S2
  ) / (
    sum(df_w_veps_est$n) - length(df_w_veps_est$n)
  )

  var_a_hat <- var_eps_hat * mean((n_t[n_t != 0])^(-1))

  # --------------------------------------------------------------------------
  # Apply ARMAN method for latent parameter point estimates
  # --------------------------------------------------------------------------

  wong_est <- wong(
    ar_hat = phi_hat,
    ma_b_hat = theta_b_hat,
    v_b_hat = var_b_hat,
    v_a_hat = var_a_hat,
    theta_free = arma_order[2] == 1
  )

  theta_hat <- wong_est[1]
  var_eta_hat <- wong_est[2]


  # --------------------------------------------------------------------------
  # Estimate Time Series Covariance Matrix
  # --------------------------------------------------------------------------

  G_hat <- fG(
    size = T,
    eta_variance = var_eta_hat,
    ar = phi_hat,
    ma = theta_hat
  )

  # --------------------------------------------------------------------------
  # Estimate Time Series and calculate likelihood
  # --------------------------------------------------------------------------

  u_hat <- f_u_hat(
    n_t,
    df_w$w,
    G_hat,
    var_eps_hat
  )

  u_hat_initial = u_hat

  observed_days <- n_t != 0
  G_obs <- G_hat[observed_days, observed_days]

  l2 <- -1 / 2 * (
    T * log(2 * pi) +
      as.numeric(
        determinant(
          G_obs,
          logarithm = TRUE
        )$modulus
      ) +
      as.vector(
        t(u_hat) %*%
          solve(G_obs) %*%
          u_hat
      )
  )

  # Current likelihood
  l_new <- l2 + l1
  l_old <- l_new + 2 * tol

  # --------------------------------------------------------------------------
  # Set up vectors to track parameter estimates and likelihoods
  # --------------------------------------------------------------------------

  l1_track <- l1
  l2_track <- l2
  l_track <- l_old

  phi_track <- phi_hat
  theta_track <- theta_hat
  var_eta_track <- var_eta_hat
  var_eps_track <- var_eps_hat



  # --------------------------------------------------------------------------
  # REMARCS optimization loop
  # --------------------------------------------------------------------------
  updates = 1
  while (abs(l_old - l_new) > tol & updates <= max_updates) {

    # --------------------------------------------------------------------------
    # track updates
    # --------------------------------------------------------------------------
    updates = updates + 1
    l_old <- l_new


    # --------------------------------------------------------------------------
    # Generate transformed eigendecomposition to facilitate later calculations
    # NOTE: This may prove later to be unnecessary
    # --------------------------------------------------------------------------
    NGN_eigen <- eigen(
      N^(1 / 2) %*% G_hat %*% N^(1 / 2)
    )

    NGN_evec <- NGN_eigen$vectors
    NGN_eval <- NGN_eigen$values
    L <- diag(NGN_eval)

    V <- f_V(n_t)

    # --------------------------------------------------------------------------
    # Extract time series estimate and refit mixed effect model to update conditional likelihood
    # Eventually unnecessary if conditioning on aggregate values instead of time series
    # --------------------------------------------------------------------------
    dat$less_u_hat <-
      dat[[resp_var]] -
      as.vector(X_t[, observed_days] %*% u_hat)

    new_formula = formula(mod)
    new_formula[[2]] = quote(less_u_hat)
    re_new = lme4::lmer(data = dat, formula = new_formula)

    l1 <- as.numeric(stats::logLik(re_new))



    #Sparse versions of the large design matrices.
    X_t_sparse <- Matrix::Matrix(X_t, sparse = TRUE)
    X_r_sparse <- Matrix::Matrix(X_r, sparse = TRUE)

    # ------------------------------------------------------------------------
    # Woodbury calculations are used here to lessen calculation time
    # ------------------------------------------------------------------------

    G_inv <- chol2inv(chol(G_hat))

    Gplus_inv <- chol2inv(
      chol(G_inv + N / var_eps_hat)
    )

    B_inv <- (solve(var_re) + Matrix::t(X_r) %*% X_r / var_eps_hat - var_eps_hat ^ (-2) * Matrix::t(X_r) %*% X_t_sparse %*% Gplus_inv %*% Matrix::t(X_t_sparse) %*% X_r_sparse) #|>
    B_inv <- chol2inv(chol(B_inv))

    beta_hat_2 = estimate_beta_gls(y = y,
                                   X_f = X_f_m,
                                   X_r = X_r_sparse,
                                   X_t = X_t_sparse,
                                   Sigma_b = var_re,
                                   Gplus_inv = Gplus_inv,
                                   sigma_eps2 = var_eps_hat)
    beta_hat = beta_hat_2$beta_hat
    # ------------------------------------------------------------------------
    # Update random effect intercepts and variance estimates
    # ------------------------------------------------------------------------

    b_hat_list = estimate_b(y = y,
                            X_f = X_f_m,
                            X_r = X_r_sparse,
                            X_t = X_t_sparse,
                            beta_hat = beta_hat,
                            Sigma_b = var_re,
                            Gplus_inv = Gplus_inv,
                            sigma_eps2 = var_eps_hat)

    b_hat <- b_hat_list$b_hat
    B <- b_hat_list$B

    B_inv <- solve(B)
    var_re_update <- update_Sigma_b(b_hat = b_hat, B_inv = B_inv, mod = mod)


    var_re <- var_re_update$matrix



    # ------------------------------------------------------------------------
    # Update ARMA-Noise Process Estimate
    # ------------------------------------------------------------------------


    dat$resid <-
      as.vector(
        dat[[resp_var]] -
          X_f_m %*% beta_hat - X_r %*% b_hat
      )

    # ------------------------------------------------------------------------
    # Repeat fitting of ARMA-Noise process
    # ------------------------------------------------------------------------


    df_w <- dat |>
      dplyr::group_by(t) |>
      dplyr::summarise(
        w = mean(resid),
        S2 = stats::var(resid),
        n = dplyr::n(),
        .groups = "drop"
      )

    df_w <- dplyr::left_join(
      tibble::tibble(t = seq_len(max(df_w$t))),
      df_w,
      by = "t"
    )

    df_w$n <- tidyr::replace_na(df_w$n, 0)

    arma_w <- forecast::Arima(
      df_w$w,
      order = c(1, 0, 1),
      include.mean = FALSE,
      #init = c(phi_hat, theta_b_hat),
      method = "ML"
    )

    phi_hat <- arma_w$coef["ar1"]
    theta_b_hat <- arma_w$coef["ma1"]
    var_b_hat <- arma_w$sigma2



    df_w_veps_est <- df_w |>
      dplyr::filter(n > 1)

    var_eps_hat <- sum(
      (df_w_veps_est$n - 1) * df_w_veps_est$S2
    ) / (
      sum(df_w_veps_est$n) - length(df_w_veps_est$n)
    )

    var_a_hat <- var_eps_hat * mean((n_t[n_t != 0])^(-1))

    wong_est <- wong(
      phi_hat,
      theta_b_hat,
      var_b_hat,
      var_a_hat,
      theta_free = arma_order[2] == 1
    )

    theta_hat <- wong_est[1]
    var_eta_hat <- wong_est[2]

    # ------------------------------------------------------------------------
    # Update estimates of time series and corresponding covariance matrix
    # ------------------------------------------------------------------------

    G_hat <- fG(
      size = T,
      eta_variance = var_eta_hat,
      ar = phi_hat,
      ma = theta_hat
    )

    u_hat <- f_u_hat(
      n_t,
      df_w$w,
      G_hat,
      var_eps_hat
    )

    G_obs <- G_hat[observed_days, observed_days]

    # ------------------------------------------------------------------------
    # Update level 2 likelihood
    # ------------------------------------------------------------------------


    l2 <- -1 / 2 * (
      T * log(2 * pi) +
        as.numeric(
          determinant(
            G_obs,
            logarithm = TRUE
          )$modulus
        ) +
        as.vector(
          t(u_hat) %*%
            solve(G_obs) %*%
            u_hat
        )
    )

    # cat("l2 = ", l2, "\n")

    l_new <- l2 + l1

    # ------------------------------------------------------------------------
    # Store parameter estimates and likelihoods
    # ------------------------------------------------------------------------


    l1_track <- append(l1_track, l1)
    l2_track <- append(l2_track, l2)
    l_track <- append(l_track, l_new)


    phi_track <- append(phi_track, phi_hat)
    theta_track <- append(theta_track, theta_hat)
    var_eta_track <- append(var_eta_track, var_eta_hat)
    var_eps_track <- append(var_eps_track, var_eps_hat)

  }

  # --------------------------------------------------------------------------
  # Asymptotic covariance of time-series parameter estimates
  # --------------------------------------------------------------------------

  S_o <- Sigma_o_varyingn(
    phi_hat,
    theta_b_hat,
    var_eps_hat,
    var_b_hat,
    n_t
  )

  wong_est <- wong(
    phi_hat,
    theta_b_hat,
    var_b_hat,
    var_eps_hat / mean(n_t[n_t != 0]),
    theta_free = arma_order[2] == 1
  )

  mean_inv_n = mean(n_t[n_t != 0])

  theta_hat <- wong_est[1]
  var_eta_hat <- wong_est[2]
  var_eta_est <- var_eta_hat


  if (arma_order[2] == 1){
    H1 <- dHf_dtau1(
      theta_hat,
      var_eta_hat
    )

    H0 <- dHf_dtau0(
      phi_hat,
      theta_b_hat,
      var_a_hat,
      var_b_hat
    )

    GGG <- -solve(H1) %*% H0
    S_f <- GGG %*% S_o %*% Matrix::t(GGG)
    S_l <- S_f[c(1, 5, 6), c(1, 5, 6)]
    S_l_plot <- S_l / T

    ts.pars <- c(
      phi_hat,
      theta_hat
    )

    names(ts.pars) <- c(
      "phi",
      "theta"
    )

    ts.pars <- cbind(
      ts.pars,
      sqrt(diag(S_l_plot)[1:2])
    )

    pvals_ts <- 2 * (
      1 - stats::pnorm(
        abs(ts.pars[, 1] / ts.pars[, 2])
      )
    )

    ts.pars <- cbind(
      ts.pars,
      pvals_ts
    )

    print(ts.pars)

  } else{
    #var_eta_est = max((1 + theta_b_hat ^ 2) * var_b_hat - (1 + phi_hat ^ 2) * var_a_hat, 0)
    GG = matrix(0, nrow = 3, ncol = 4)
    GG[1,1] = 1
    GG[2,4] = 1
    GG[3,1] = -2 * phi_hat * var_a_hat
    GG[3,2] = 2 * theta_b_hat * var_b_hat
    GG[3,3] = 1 + theta_b_hat ^ 2
    GG[3,4] = -(1 + phi_hat ^ 2)
    S_f = GG %*% S_o %*% t(GG)

    S_l_plot = S_f / T
    #var_eta_hat = var_eta_est
    #theta_hat = 0

    ts.pars <- c(
      phi_hat
    )

    names(ts.pars) <- c(
      "phi"
    )

    ts.pars <- cbind(
      ts.pars,
      sqrt(diag(S_l_plot)[c(1)])
    )

    pvals_ts <- 2 * (
      1 - stats::pnorm(
        abs(ts.pars[, 1] / ts.pars[, 2])
      )
    )

    ts.pars <- cbind(
      ts.pars,
      pvals_ts
    )

    print(ts.pars)
  }








  Sigma_beta = var_beta(X_f,
                        X_r,
                        X_t,
                        Sigma_b = var_re,
                        Gplus_inv = Gplus_inv,
                        sigma_eps2 = var_eps_hat)




  # --------------------------------------------------------------------------
  # Final estimates and fitted values
  # --------------------------------------------------------------------------

  beta_hat <- cbind(
    beta_hat,
    sqrt(diag(Sigma_beta))
  )

  pvals <- 2 * (
    1 - stats::pnorm(
      abs(beta_hat[, 1] / beta_hat[, 2])
    )
  )

  beta_hat <- cbind(
    beta_hat,
    pvals
  )

  AIC <- 2 * (nrow(beta_hat) + 3) - 2 * l_new

  fitted.mat <- data.frame(
    Day = dat$t,
    Reg = X_f %*% beta_hat[, 1],
    TS = X_t[, observed_days] %*% u_hat
  )


  m1 <- nlevels(getME(mod, "flist")$adm1)
  m2 <- nlevels(getME(mod, "flist")$`adm1:adm2`)

  ranef_final = ranef(mod)
  ranef_final$adm1 = b_hat[(m2 + 1):(m1 + m2)]
  ranef_final$adm1 = as.matrix(ranef_final$adm1)
  ranef_final$`adm1:adm2` = b_hat[(1):(m2)]
  ranef_final$`adm1:adm2` = as.matrix(ranef_final$`adm1:adm2`)

  rownames(ranef_final$adm1) = rownames(ranef(mod)$adm1)
  rownames(ranef_final$`adm1:adm2`) = rownames(ranef(mod)$`adm1:adm2`)

  res.list <- list(
    beta_hat,
    b_hat = ranef_final,
    var_b = var_re_update$variances,
    ts.pars,
    l1_track,
    l2_track,
    AIC,
    fitted.mat,
    var_eta_hat,
    var_eps_hat,
    u_hat_initial
  )

  dat$final_resid = dat[[resp_var]] - fitted.mat[,2] - fitted.mat[,3]
  final_resid_daily = dat |>
    group_by(t) |>
    summarize(mean_resid = mean(final_resid))

  final_resid_daily = left_join(tibble(t = 1:max(final_resid_daily$t)), final_resid_daily)

  print(acf(final_resid_daily$mean_resid,na.action = na.pass, main = "ACF of Final Aggregated Residuals Aggregated"))


  names(res.list) <- c(
    "Beta_Pars",
    "Random Effects",
    "Random Effect Variances",
    "TS_Pars",
    "level 1 log(likelihood)",
    "level 2 log(likelihood)",
    "AIC",
    "Fitted_Values",
    "Time Series Variance",
    "Individual Variance",
    "Initial Time Series Estimate"
  )
  ans2 = readline("Would you like to refit with a latent AR(1) process?
                  [1] Yes
                  [2] No")

  if (ans2 == 1){
    return(RCSmixed.fit(mod = mod,
                        dat = dat,
                        resp_var = resp_var,
                        tol = tol,
                        max_updates = max_updates,
                        arma_order = c(1,0)))
  }else{
    return(res.list)
  }
}


