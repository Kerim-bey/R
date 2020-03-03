line_norm <- function(center,A)
{
  det<-det(A)
  a <- A[2,2]/det
  b <- -A[2,1]/det
  c <- -A[1,2]/det
  d <- A[1,1]/det
  
  x0 <- center[1]
  y0 <- center[2]
  
  X <- seq(-2.5, 2.5, 0.1)
  Y <- seq(-2.5, 2.5, 0.1)
  
  
  A <- d
  B <- a
  C <- -c -b
  D <- -2*d*x0 + y0*(c+b)
  E <- -2*a*y0 + x0*(c+b)
  F <- d*x0^2 + a*y0^2 + x0*y0*(-c-b)
  
  func <- function(x, y) {
    1/(2*pi*sqrt(det))*exp((-1/2)*(x^2*A + y^2*B + x*y*C + x*D + y*E + F))
  }
  Z <- outer(X, Y, func)
  
  contour(X, Y, Z)
}
par(pty="s")

#line_norm(c(0,0),matrix(c(1,0,0,1),2,2))
#line_norm(c(0,0),matrix(c(0.3,0,0,1),2,2))
line_norm(c(0,0),matrix(c(1,1,0,1),2,2))