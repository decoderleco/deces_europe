library(gsl) # pour avoir lambert_W0 et lambert_Wm1

memoize <- function (def, fun, lng) {
	if (missing(lng)) {
		lng <- 16
	}
	memo <- rep(NA, max(lng, 1))
	function (j) {
		if (j < 0) {
			return (def)
		}
		i <- j + 1
		ml <- length(memo)
		if (ml <= i) {
			memo[max(2 * ml, i + 1)] <<- NA
		}
		if (is.na(memo[i])) {
			v <- fun(j)
			if (is.nan(v) | is.infinite(v)) {
				memo[i] <<- 0
			} else {
				memo[i] <<- v
			}
		} 
		memo[i]
	}
}

validation <- function(msg, cond) {
	if (! cond) {
		stop(msg, call. = FALSE)
	}
}

sird <- function (beta, gamma, delta, S0, nbJours, N) {
	validation(paste("beta", beta), 0 <= beta & beta <= 1)
	validation("gamma", 0 <= gamma & gamma <= beta)
	validation("delta", 0 <= delta & delta <= gamma)
	validation("S0", 0 <= S0)
	validation("nbJours", 2 <= nbJours)
	validation("N", 2 <= N)
	
	epsilon <- (nbJours - 1) / (N - 1)
	
	# population saine
	fS <- memoize(S0,
			function (k) {
				if (k == 0) {
					S0
				} else {
					fS(k - 1) - epsilon * beta * fI(k - 1) * fS(k - 1) / S0
				}
			}
	)
	
	# population infectée
	fI <- memoize(0,
			function (k) {
				if (k == 0) {
					1
				} else {
					fI(k - 1) + epsilon * (beta * fI(k - 1) * fS(k - 1) / S0 - gamma * fI(k - 1))
				}
			}
	)
	
	# population rétablie
	fR <- memoize(0,
			function (k) {
				if (k <= 0) {
					0
				} else {
					fR(k - 1) + epsilon * (gamma - delta) * fI(k - 1)
				}
			}
	)
	
	# population décédée
	fD <- function (k) {
		delta * fI(k)
	}
	
	tab <- function (f) {
		te <- numeric(N)
		for (i in 1:N) {
			k <- i - 1
			te[i] <- f(k)
		}
		t <- numeric(nbJours)
		for (j in 1:(nbJours - 1)) {
			k <- floor(j / epsilon)
			jd <- j / epsilon - k
			t[j] <- te[k] * (1 - jd) + te[k + 1] * jd
		}
		t[nbJours] <- te[N]
		t
	}
	
	S <- tab(fS)
	I <- tab(fI)
	R <- tab(fR)
	D <- tab(fD)
	list(beta = beta, gamma = gamma, S0 = S0, nbJours = nbJours, N = N, epsilon = epsilon, S = S, I = I, R = R, D = D)
}

passeBasP <- function(t, rc) {
	a <- 1 / (1 + rc)
	res <- numeric(length(t))
	if (length(t) == 0) return (res)
	res[1] <- a * t[1]
	if (length(t) == 1) return (res)
	for (i in 2:length(t)) {
		res[i] <- a * t[i - 1] + (1 - a) * res[i - 1]
	}
	res
}

passeBasN <- function(t, rc) {
	a <- 1 / (1 + rc)
	lng <- length(t)
	res <- numeric(lng)
	if (lng == 0) return (res)
	res[lng] <- a * t[lng]
	if (lng == 1) return (res)
	for (i in (lng - 1):1) {
		res[i] <- a * t[i + 1] + (1 - a) * res[i + 1]
	}
	res
}

passeBas <- function(t, rc) {
	k <- sqrt(rc)
	passeBasN(passeBasP(t, k), k)
}

maxX <- function (t) {
	validation("maxX: length(t) == 0", length(t) > 0)
	x <- 1
	m <- t[x]
	for (i in 2:length(t)) {
		if (m < t[i]) {
			m <- t[i]
			x <- i
		}
	}
	x
}

chercheVague <- function (deces, seuil) {
	validation("chercheVague: length(t) == 0", length(t) > 0)
	if (missing(seuil)) seuil <- 10
	if (is.null(deces)) {
		return (NULL)
	}
	d <- 1
	f <- length(deces)
	lDeces <- passeBas(deces, seuil)
	mx <- d + maxX(lDeces[d:f])
	dx <- max(d, mx - 1)
	m <- lDeces[dx]
	if (dx > d) {
		for (i in (mx - 2):d) {
			if (lDeces[i] <= m) {
				m <- lDeces[i]
				dx <- i
			}
		}
	}
	fx <- min(mx + 1, f)
	m <- lDeces[fx]
	if (fx < f) {
		for (i in (mx + 2):f) {
			if (m >= lDeces[i]) {
				m <- lDeces[i]
				fx <- i
			}
		}
	}
	if (mx == dx | mx == fx | mean(deces[dx:fx]) < 1) {
		return (NULL)
	}
	deces[dx:fx]
}

lissageG <- function(tab, d) {
	r <- ceiling(d / 2)
	dn <- dnorm(seq(-r, r, by = 1), 0, sqrt(d))
	l <- length(tab)  
	res <- numeric(l)
	for (i in 1:l) {
		b <- max(1, i - r)
		e <- min(l, i + r)
		v <- 0
		s <- 0
		for (k in b:e) {
			v <- v + dn[k - i + r + 1] * tab[k]
			s <- s + dn[k - i + r + 1]
		}
		res[i] <- v / s
	}
	res
}

ecart <- function(t, mxt, s) {
	mxs <- maxX(s)
	dxs <- mxs - mxt
	if (dxs + 1 <= 0) {
		ss <- c(rep(0, -dxs), s[1:(dxs + length(t))])
	} else {
		ss <- s[(dxs + 1):(dxs + length(t))]
	}
	res <- 0
	maxT <- max(lissageG(t, 10))
	totT <- sum(t)
	for (i in 1:length(t)) {
		res <- res + (100 * (t[i] - ss[i]) / maxT) ^ 2
	}
	sqrt(res / length(t))
}

ecartDc <- function(t, mxt, s) {
	mxs <- maxX(s)
	dxs <- mxs - mxt
	if (dxs + 1 <= 0) {
		ss <- c(rep(0, -dxs), s[1:(dxs + length(t))])
	} else {
		ss <- s[(dxs + 1):(dxs + length(t))]
	}
	abs(sum(ss) / sum(t) - 1)
}

devineParams <- function(deces, pop) {
	if (is.null(deces)) {
		return (NULL)
	}
	e <- exp(1)
	S0 <- pop
	D = lissageG(deces, 10)
	mx <- maxX(D)  
	Dm <- D[mx]
	iDm <- sum(deces[0:mx])
	iDoo <- sum(deces)
	
	valide <- function (p) {
		0 < p$beta & p$beta <= 1 & 0 < p$gamma & p$gamma <= p$beta & 0 <= p$delta & p$delta <= p$gamma & p$beta / p$gamma > 1
	}
	
	params <- function (Dm, iDm, iDoo) {
		optRho <- function(rho) {
			((lambert_W0(-rho * exp(-rho)) + rho) / log(rho) - iDoo / iDm) ^ 2
		}
		
		rho <- optim(par = c(1), optRho, method = "Brent", lower = 1, upper = 20)$par
		delta <- Dm / (log(1 / rho / exp(1)) * S0 / rho + S0 + 1)
		beta <- delta * S0 / iDm * log(rho)
		gamma <- beta / rho
		p <- list(beta = beta, gamma = gamma, delta = delta, S0 = S0, err = Inf, errDc = Inf)  
		
		if (valide(p)) {
			sirD <- sird(beta, gamma, delta, S0, 1000, 1000)$D
			p$err <- ecart(D, mx, sirD)
			p$errDc <- ecartDc(deces, mx, sirD)
			if (is.na(p$err)) {
				p$err <- Inf
			}
		}
		p
	}
	
	pM <- params(Dm, iDm, iDoo)
	maxErrDc <- 0.05
	resolution <- 100
	epsilon <- iDoo / resolution
	iM <- 0
	for (i in 1:resolution) {
		iDoo <- iDoo + epsilon
		p <- params(Dm, iDm, iDoo)
		if (valide(p) & p$errDc < maxErrDc & p$err < pM$err) {
			pM <- p
			iM <- i
		}
		iDoo <- iDoo + epsilon
	}
	if (valide(pM) & pM$errDc < maxErrDc) {
		pM
	} else {
		NULL
	}
}

afficheSim <- function (deces, pop) {
	vague <- chercheVague(deces)
	if (is.null(vague)) {
		print("aucune vague")
		return (FALSE)
	}
	params <- devineParams(vague, pop)
	if (is.na(params$beta)) {
		print("aucun paramètre trouvé")
		return (FALSE)
	}
	lDeces <- lissageG(vague, 10)
	mx <- maxX(lDeces)
	lng <- length(lDeces)
	rho <- params$beta / params$gamma
	bMax <- log(1 + params$S0 - params$S0 / rho * (log(rho) + 1)) / (params$beta - params$gamma)
	lngSim <- bMax + lng
	lngSimDecale <- 0
	while (lngSimDecale < lng) {
		sim <- sird(params$beta, params$gamma, params$delta, params$S0, lngSim, lngSim)$D
		dxs <- maxX(sim) - mx
		if (dxs + 1 <= 0) {
			simDecale <- c(rep(0, -dxs), sim[1:(dxs + lng)])
		} else {
			simDecale <- sim[(dxs + 1):(dxs + lng)]
		}
		lngSim <- lngSim * 2
		lngSimDecale <- length(simDecale)
	}
	plot(lDeces, type = "l", col = "red")
	lines(simDecale)
	lines(abs(simDecale - lDeces), col = "blue")
	legend("topleft", c("décès", "SIRD", "erreur"), col = c("red", "black", "blue"), lty = c(1, 1, 1))  
}

if (TRUE) {
	# exemple simple d'utilisation
	decesFr <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
			0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 1, 0, 3, 2, 7, 3, 6, 8, 15, 0, 31, 12, 0, 58, 0, 0, 95, 207, 112, 113, 186, 240,
			231, 364, 299, 318, 294, 418, 500, 1252, 609, 1122, 1053, 519, 833, 1416, 544, 1340, 985, 636, 561, 573, 743, 1438,
			753, 761, 641, 389, 547, 528, 544, 516, 390, 369, 242, 437, 367, 426, 289, 217, 197, 135, 306, 333, 275, 179, 242, 80,
			70, 263, 348, 85, 351, 102, 98, 483, 129, 0, 0, 0, 124, 86, 0, 9, 137, 1, 67, 51, 57, 31, 32, 106, 81, 45, 45, 31, 13, 55, 88,
			21, 27, 28, 24, 10, 28, 114, 25, 28, 14, 18, 6, 24, 54, 13, 19, 26, 1, 0, 35, 30, 19, 13, 18, 0, 0, 29, 11, 31, 16, 26, 3, 0, 23,
			2, 88, 17, 13, 3, 0, 24, 0, 0, 4, 10, 0, 0, 18, 13, 14, 16, 11, 0, 0, 30, 2, 11, 4, 13, 1, 0, 15, 14, 18, 17, 19, 1, 1, 20, 22, 18,
			11, 24, 9, 0, 16, 19, 0, 35, 18, 6, 3, 31, 27, 27, 17, 0, 0, 0, 18, 38, 32, 19, 82, 18, 5, 34, 50, 48, 47, 154, 25, 11, 53, 79,
			48, 46, 151, 39, 27, 84, 73, 80, 57, 136, 25, 32, 71, 87, 80, 73, 110, 35, 46, 97, 128, 106, 85, 179, 73, 85, 148, 281, 168,
			154, 300, 136, 0, 368, 529, 259, 232, 547, 222, 231, 428, 1279, 0, 323, 829, 303, 270, 560, 1231, 328, 415, 933, 353,
			303, 511, 1226, 431, 417, 1141, 252, 214, 506, 1011, 388, 331, 959, 212, 198, 409, 777, 311, 324, 630, 212, 174, 366,
			838, 297, 296, 628, 193, 150, 376, 791, 290, 261, 612, 189, 131, 354, 802, 277, 291, 159, 146, 173, 368, 969, 304, 251,
			133, 156, 116, 385, 868, 282, 276, 591, 168, 151, 312, 742, 229, 284, 636, 193, 141, 404, 656, 310, 347, 649, 230, 172,
			446, 614, 350, 344, 821, 241, 195, 456, 726, 358, 0, 1008, 191, 171, 460, 724, 296, 360, 645, 199, 167, 412, 587, 310,
			271, 572, 157, 159, 334, 431, 278, 261, 539, 185, 122, 375, 418, 322, 293, 439, 170, 130, 363, 365, 252, 277, 223, 169,
			207, 341, 400, 87, 422, 285, 177, 138, 343, 288, 246, 225, 897, 191, 131, 360, 381, 303, 308, 332, 1, 0)
	
	essai <- es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
			filter(numerosemaine>400) %>%
			filter(geo == "FR")
	
	decesFr <- essai$deces_tot-10844
	
	popFr <- 68147687
	
	vague <- chercheVague(decesFr)
	
	params <- devineParams(vague, popFr)
	
	simD <- sird(params$beta, params$gamma, params$delta, params$S0, 36, 36)$D
	
	plot(lissageG(vague, 10), type = "l", col = "red")	
	lines(c(rep(0, 20), simD))
}

