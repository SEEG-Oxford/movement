# calculate flux between two points
flux <- function(i, j, distance, population, p = 1, minpop = 1,
                 maxrange = Inf) {
  # given the indices $i$ and $j$, vector of population sizes
  # 'population', (dense) distance matrix 'distance' and
  # proportion of all inhabitats commuting 'p' (by default 1 - everybody)
  # calculate $T_{ij}$, the number of people travelling between
  # $i$ and $j$. If the pairwise distance is greater than 'max' it is
  # assumed that no travel occurs between these points. This can speed
  # up the model.
  
  # get the population sizes $m_i$ and $n_j$
  m_i <- population[i]
  n_j <- population[j]
  
  # if the population at the centre is below the minimum,
  # return 0 (saves some calculation time)
  if (m_i < minpop) return (0)
  
  # calculate the total number of people commuting from i -
  # the proportion (p) multiplied by the population $m_i$
  T_i <- m_i * p
  
  # look up $r_{ij}$ - the euclidean distance between $i$ and $j$
  r_ij <- distance[i, j]
  
  # if it's beyond the maximum range return 0
  if (r_ij > maxrange) return (0)
  
  # get indices of points within this range
  in_radius <- distance[i, ] <= r_ij
  
  # sum the total population in this radius (excluding i & j)
  
  # calculate $s_{ij}$, the total population in the search radius
  # (excluding $i$ and $j$)
  
  # which to include in the sum
  pop_sum_idx <- in_radius
  # not i or j
  pop_sum_idx[c(i, j)] <- FALSE
  # get sum
  s_ij <- sum(population[pop_sum_idx])
  
  # if the sum is 0 (no populations in that range) return 0 movement
  if (s_ij == 0) return (0)
  
  # calculate the number of commuters T_{ij} moving between sites
  # $i$ and $j$ using equation 2 in Semini et al. (2013)
  T_ij <- T_i * (m_i * n_j) / (m_i + s_ij) * (m_i + n_j + s_ij)
  
  # return this
  return (T_ij)
}

# run radiation model
radiation <- function(distance, population, p = 1,
                      minpop = 0, maxrange = Inf,
                      progress = TRUE) {
  # Given a euclidean distance matrix 'distance', a vector
  # 'population' giving population sizes for the nodes and the
  # proportion of the total population (all nodes) commuting 'p'
  # use Semini et al. (2013)'s (original) radiation model to
  # predict movement between nodes. The model can be restricted
  # to a maximum range 'maxrange', (beyond which people are assumed not
  # to travel). A minimum cell population size can aslo be set via 'minpop'.
  # If the cell at the centre of the radius has a population less than minpop
  # the cell is considered unimportant and a 0 returned. This can save some
  # computation time. If 'progress = TRUE' a text progress bar is displayed
  # and starting and total times reported.
  
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
                 p = p,
                 minpop = minpop,
                 maxrange = maxrange)
    
    # and stick it in the results matrix
    movement[pair[1], pair[2]] <- movement[pair[2], pair[1]] <- T_ij
    
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
