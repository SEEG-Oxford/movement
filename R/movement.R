# calculate flux between two points using the radiation model
radiation.flux <- function(i, j, distance, population,
                           theta = c(1), symmetric = FALSE,
                           minpop = 1, maxrange = Inf) {

  # get model parameters
  p <- theta[1]
  
  # get the population sizes $m_i$ and $n_j$
  m_i <- population[i]
  n_j <- population[j]

  # if the population at the centre is below the minimum,
  # return 0 (saves some calculation time)
  if (m_i < minpop | n_j < minpop) {
  # if it's symmetric return one 0
    if (symmetric) return (0)
    # otherwise return two
    else return (c(0, 0))
  }

  # calculate the total number of people commuting from i -
  # the proportion (p) multiplied by the population $m_i$
  T_i <- m_i * p
  
  # and from j
  T_j <- n_j * p
  
  # look up $r_{ij}$ - the euclidean distance between $i$ and $j$
  r_ij <- distance[i, j]

  # if it's beyond the maximum range return 0
  if (r_ij > maxrange) {
    # if it's symmetric return one 0
    if (symmetric) return (0)
    # otherwise return two
    else return (c(0, 0))
  }

  # get indices of points within this range
  i_in_radius <- distance[i, ] <= r_ij
  j_in_radius <- distance[j, ] <= r_ij
  
  # sum the total population in this radius (excluding i & j)

  # calculate $s_{ij}$, the total population in the search radius
  # (excluding $i$ and $j$)

  # which to include in the sum
  i_pop_sum_idx <- i_in_radius
  # not i or j
  i_pop_sum_idx[c(i, j)] <- FALSE
  # get sum
  i_s_ij <- sum(population[i_pop_sum_idx])
  
  # which to include in the sum
  j_pop_sum_idx <- j_in_radius
  # not i or j
  j_pop_sum_idx[c(i, j)] <- FALSE
  # get sum
  j_s_ij <- sum(population[j_pop_sum_idx])

#   # if the sum is 0 (no populations in that range) return 0 movement
#   if (i_ s_ij == 0) return (0)

  # calculate the number of commuters T_{ij} moving between sites
  # $i$ and $j$ using equation 2 in Semini et al. (2013)
  m_i_times_n_j <- m_i * n_j
  m_i_plus_n_j <- m_i + n_j
  
  T_ij <- T_i * m_i_times_n_j / (m_i + i_s_ij) * (m_i_plus_n_j + i_s_ij)
  
  # and in the opposite direction
  T_ji <- T_j * m_i_times_n_j / (n_j + j_s_ij) * (m_i_plus_n_j + j_s_ij)
  
  
  # return this
  if (symmetric) return (T_ij + T_ji)
  else return (c(T_ij, T_ji))
}


# calculate flux between two points according to a classic gravity model
gravity.flux <- function(i, j, distance, population,
                         theta = c(1, 0.6, 0.3, 3),
                         symmetric = FALSE,
                         minpop = 1, maxrange = Inf) {
  # given the indices $i$ and $j$, vector of population sizes
  # 'population', (dense) distance matrix 'distance', vector of parameters
  # 'theta' in the order [scalar, exponent on donor pop, exponent on recipient
  # pop, exponent on distance], calculate $T_{ij}$, the number of people
  # travelling between $i$ and $j$. If the pairwise distance is greater than
  # 'max' it is assumed that no travel occurs between these points. This can
  # speed up the model.
  
  # get the population sizes $m_i$ and $n_j$
  m_i <- population[i]
  n_j <- population[j]
  
  # if the population at the centre is below the minimum,
  # return 0 (saves some calculation time)
  m_i[m_i < minpop] <- 0
  
  # look up $r_{ij}$ - the euclidean distance between $i$ and $j$
  r_ij <- distance[i, j]
  
  # if it's beyond the maximum range return 0 - this way to vectorize with ease...
  r_ij[r_ij > maxrange] <- 0
  
  # calculate the number of commuters T_{ij} moving between sites
  # $i$ and $j$ using equation 1 in Viboud et al. (2006)
  T_ij <- theta[1] * (m_i ^ theta[2]) * (n_j ^ theta[3]) / (r_ij ^ theta[4])
  
  # and the opposite direction
  T_ji <- theta[1] * (n_j ^ theta[2]) * (m_i ^ theta[3]) / (r_ij ^ theta[4])
  
  # return this
  if (symmetric) return (T_ij + T_ji)
  else return (c(T_ij, T_ji))
}


# fit a movement model
movement.model <- function(distance, population,
                           flux = radiation.flux,
                           symmetric = FALSE,
                           progress = TRUE,
                           ...) {

  # create a movement matrix in which to store movement numbers
  movement <- matrix(NA,
                     nrow = nrow(distance),
                     ncol = ncol(distance))
  # set diagonal to 0
  movement[col(movement) == row(movement)] <- 0

  # get the all $i, j$ pairs
  indices <- which(upper.tri(distance), arr.ind = TRUE)

  # set up optional text progress bar
  if (progress) {
    start <- Sys.time()
    cat(paste('started processing at',
              start,
              '\n\nprogress:\n\n'))

    bar <- txtProgressBar(min = 1,
                          max = nrow(indices),
                          style = 3)
  }

  for (idx in 1:nrow(indices)) {
    # for each array index (given as a row of idx), get the pair of nodes
    pair <- indices[idx, ]
    i <- pair[1]
    j <- pair[2]

    # calculate the number of commuters between them
    T_ij <- flux(i = pair[1],
                 j = pair[2],
                 distance = distance,
                 population = population,
                 symmetric = symmetric,
                 ...)

    # and stick it in the results matrix
    
    # if the symmetric distance was calculated (sum of i to j and j to i)
    # stick it in both triangles
    if (symmetric) {
      
      movement[pair[1], pair[2]] <- movement[pair[2], pair[1]] <- T_ij
      
    } else {
      
      # otherwise stick one in the upper and one in the the lower
      # (flux returns two numbers in this case)
      # i.e. rows are from (i), columns are to (j)
      movement[pair[1], pair[2]] <- T_ij[1]
      movement[pair[2], pair[1]] <- T_ij[2]
      
    }

    if (progress) setTxtProgressBar(bar, idx)

  }

  if (progress) {
    end <- Sys.time()

    cat(paste('\n\nstarted processing at',
              start,
              '\n\ntime taken:',
              round(end - start),
              'seconds\n'))

    close(bar)
  }

  return (movement)
}

# 
# 
# # run gravity model
# gravity.movement <- function(distance, population, theta,
#                              minpop = 0, maxrange = Inf,
#                              progress = TRUE) {
#   # Given a euclidean distance matrix 'distance', a vector
#   # 'population' giving population sizes for the nodes and
#   # a set of parameters 'theta',
#   # use Viboud et al. (2006)'s gravity model to
#   # predict movement between nodes. The model can be restricted
#   # to a maximum range 'maxrange', (beyond which people are assumed not
#   # to travel). A minimum cell population size can aslo be set via 'minpop'.
#   # If the cell at the centre of the radius has a population less than minpop
#   # the cell is considered unimportant and a 0 returned. This can save some
#   # computation time.
# 
#   # create a movement matrix in which to store movement numbers
#   movement <- matrix(NA,
#                      nrow = nrow(distance),
#                      ncol = ncol(distance))
#   # set diagonal to 0
#   movement[col(movement) == row(movement)] <- 0
# 
#   # get the all $i, j$ pairs
#   indices <- which(upper.tri(distance), arr.ind = TRUE)
# 
#   # set up optional text progress bar
#   if (progress) {
#     start <- Sys.time()
#     cat(paste('started processing at',
#               start,
#               '\n\nprogress:\n\n'))
# 
#     bar <- txtProgressBar(min = 1,
#                           max = nrow(indices),
#                           style = 3)
#   }
# 
#   for (idx in 1:nrow(indices)) {
#     # for each array index (given as a row of idx), get the pair of nodes
#     pair <- indices[idx, ]
#     i <- pair[1]
#     j <- pair[2]
# 
#     # calculate the number of commuters between them
#     T_ij <- gravity.flux(i = pair[1],
#                          j = pair[2],
#                          distance = distance,
#                          population = population,
#                          theta=theta,
#                          minpop = minpop,
#                          maxrange = maxrange)
# 
#     # and stick it in the results matrix
#     movement[pair[1], pair[2]] <- movement[pair[2], pair[1]] <- T_ij
# 
#     if (progress) setTxtProgressBar(bar, idx)
# 
#   }
# 
#   if (progress) {
#     end <- Sys.time()
# 
#     cat(paste('\n\nstarted processing at',
#               start,
#               '\n\ntime taken:',
#               round(end - start),
#               'seconds\n'))
# 
#     close(bar)
#   }
# 
#   return (movement)
# 
# }
# 


