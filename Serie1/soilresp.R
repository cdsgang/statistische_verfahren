# Assignment Part 1
data = read.csv("./soilresp.csv", sep=';')
x = data[, "temp"]
y = data[, "resp"]
y = log(y)
plot(x, y)

b0.vec = seq(4, 6, length=100)
b1.vec = seq(0, 0.1, length=100)

# Grid search as described in the assignment
result = matrix(0, length(b0.vec), length(b1.vec))
for (i in 1:length(b0.vec)) {
  for (j in 1:length(b1.vec)) {
    for (k in 1:length(x)) {
      result[i, j] = result[i, j] + abs((b0.vec[i] + b1.vec[j] * x[k] - y[k]))
    }
  }
}

# Find correct indices
ind = which(result == min(result), arr.ind = T)
b0 = b0.vec[ind[1]]
b1 = b1.vec[ind[2]]

print(b0)
print(b1)
abline(b0, b1, col="red")

# Assignment Part 2
X = matrix(nrow = length(x), ncol = 2)
X[,1] = 1
X[,2] = x
B = c(b0, b1)

XB = X %*% B
XTX = t(X) %*% X
iXTX = solve(XTX)

# Assignment Part 3

# Quadratic
result = matrix(0, length(b0.vec), length(b1.vec))
for (i in 1:length(b0.vec)) {
  for (j in 1:length(b1.vec)) {
    for (k in 1:length(x)) {
      result[i, j] = result[i, j] + (b0.vec[i] + b1.vec[j] * x[k] - y[k])**2
    }
  }
}
ind = which(result == min(result), arr.ind = T)
b0 = b0.vec[ind[1]]
b1 = b1.vec[ind[2]]
abline(b0, b1, col="green")

# Hypercubic
result = matrix(0, length(b0.vec), length(b1.vec))
for (i in 1:length(b0.vec)) {
  for (j in 1:length(b1.vec)) {
    for (k in 1:length(x)) {
      result[i, j] = result[i, j] + (b0.vec[i] + b1.vec[j] * x[k] - y[k])**4
    }
  }
}
ind = which(result == min(result), arr.ind = T)
b0 = b0.vec[ind[1]]
b1 = b1.vec[ind[2]]
abline(b0, b1, col="blue")

# Sum of orthogonal deviances already done (red line)

# Own criterion: Use last byte of SHA256 checksum of cubic error as error
require(digest)
result = matrix(0, length(b0.vec), length(b1.vec))
for (i in 1:length(b0.vec)) {
  for (j in 1:length(b1.vec)) {
    for (k in 1:length(x)) {
      qerr = (b0.vec[i] + b1.vec[j] * x[k] - y[k])**3
      hash = as.integer(digest(qerr, algo="sha256", raw=T))
      result[i, j] = result[i, j] + hash[length(hash)]
    }
  }
}
ind = which(result == min(result), arr.ind = T)
b0 = b0.vec[ind[1]]
b1 = b1.vec[ind[2]]
abline(b0, b1, col="purple")
