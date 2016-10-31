# Theoretische Normalverteilung
x = seq(-15, 25, length = 100000)
mean = 5
sdev = sqrt(2)
gauss.density = dnorm(x, mean = mean, sd = sqrt(2))
plot(x, gauss.density)

printf = function(...) print(sprintf(...))

assignment1 = function(factor) {
  total_sum = sum(gauss.density)
  partial_sum = 0;
  for (i in 1:length(x)) {
    if ((mean - factor * sdev) < x[i] && x[i] < (mean + factor * sdev)) {
      partial_sum = partial_sum + gauss.density[i]
    }
  }
  integral = partial_sum / total_sum
  # Ich hoffe der Zeichensatz wird bei ihnen richtig angezeigt
  printf("P(μ - %dσ ≤ Y ≤ μ + %dσ) = %f", factor, factor, integral)
}

assignment1(1)
assignment1(2)
assignment1(3)

assignment2 = function(k) {
  # Stetige Gleichverteilung
  interval.a = 0
  interval.b = 1
  u.mean = (interval.b - interval.a) / 2 # Erwartungswert
  u.sdev = (interval.b - interval.a) / (2 * sqrt(3))
  # μ_Y = 1/k * k*μ_U = μ_U
  y.mean = u.mean
  # Y = 1/k(U_1 + ... + U_k)
  y.sdev = sqrt(1/k * u.sdev**2)
  # weil Var(a * X) = a**2 * Var(X) und Var(X + Y) = Var(X) + Var(Y) + Cov(X, Y) und Cov(X, Y) = 0 (hier zumindest)
  
  set.seed(42); # Deterministische Ergebnisse
  samples = 1000;
  y.mat = matrix(runif(k * samples), samples, k);
  y.vec = vector(length = samples)
  for (i in 1:length(y.mat[,1])) {
    y.vec[i] = sum(y.mat[i,]) / k
  }
  printf("k = %d ⇒", k)
  printf("μ_Y (theoretisch) = %f", y.mean)
  printf("μ_Y = %f", mean(y.vec))
  printf("Δ_Y (theoretisch) = %f", y.sdev)
  printf("ΔY = %f", sd(y.vec))
}
assignment2(5)
assignment2(10)