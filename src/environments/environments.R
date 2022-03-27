# TODO: Add comment
# 
###############################################################################

# Fonction qui permet de savoir dans quel environnement se trouve une variable ou une fonction
where <- function(name, env = caller_env()) {
	if (identical(env, empty_env())) {
		# Base case
		stop("Can't find ", name, call. = FALSE)
	} else if (env_has(env, name)) {
		# Success case
		env
	} else {
		# Recursive case
		where(name, env_parent(env))
	}
}

