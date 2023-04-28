p_game = function(pp) {
  ##### pp = probability of winning a point
  total = 0
  
  ##### win to 0, 15, or 30
  # pp^4 denotes winning four points for player X to win the game
  # negative binomial distribution for 4th success on (3+i)'th trials, but only allowing at most 2 points won by player Y
  for (i in 0:2){
    total = total + choose(3+i, i)*pp^4*(1-pp)^i
  }
  
  ##### get to 40-40
  # there must be exactly 3 points shared between the two
  deuce_p = choose(6,3)*pp^3*(1-pp)^3
  
  ##### given that deuce occurred, probability of getting another deuce (WL or LW)
  deuce_ad_p = choose(2,1)*pp*(1-pp)
  
  deuce_count = 0
  deuce_count_p = 0
  
  ##### approximates a geometric series describing winning after any number of deuces
  while(deuce_ad_p^deuce_count > 0.00001){
    # probability of getting a deuce then winning two in a row
    deuce_count_p = deuce_count_p + (deuce_ad_p^deuce_count)*pp^2
    deuce_count = deuce_count + 1
  }
  
  ##### probability of winning by deuce = getting to deuce, then winning after deuce
  win_deuce_p = deuce_p*deuce_count_p
  
  ##### overall: win without deuce + win after deuce
  total = total + win_deuce_p
  return(total)
}

p_set = function(pp) {
  pg = p_game(pp=pp)
  
  ##### pg = probability of winning a game
  total = 0
  
  ##### win 6-0, 6-1, 6-2, 6-3, 6-4
  # pg^6 denotes winning six games for player X to win the set
  # negative binomial distribution for 6th success on (5+i)'th trials, but only allowing at most 4 games won by player Y
  for (i in 0:4){
    total = total + choose(5+i,5)*pg^6*(1-pg)^i
  }
  
  ##### win 7-5
  # the first 10 games must be split 5 to 5, then player X must win 2 games in a row
  total = total + choose(10,5)*pg^7*(1-pg)^5
  
  ##### get to 6-6: tiebreak
  # the first 10 games must be split 5 to 5, then the next two must be split 1 to 1 (WL or LW only)
  tiebreak_p = (choose(10,5)*pg^5*(1-pg)^5) * (choose(2,1)*pg*(1-pg))
  
  tiebreak_total_p = 0
  
  ##### TIEBREAK
  # winning 7-0, 7-1, 7-2, 7-3, 7-4, or 7-5 in tiebreak
  tiebreak_straight_p = 0
  # negative binomial distribution for 7th success on (6+i)'th trials, but only allowing at most 5 games won by player Y
  for(w in 0:5){
    tiebreak_straight_p = tiebreak_straight_p + choose(6+w,w)*pp^7*(1-pp)^w
  }
  tiebreak_total_p = tiebreak_straight_p
  
  ##### getting to 6-6 and then winning by 2 points in a tiebreak
  # get to 6-6
  tiebreak_deuce_p = choose(12,6)*pp^6*(1-pp)^6
  # probability of getting back to equal points
  tiebreak_ad_deuce_p = choose(2,1)*pp*(1-pp)
  tiebreak_deuce_count = 0
  tiebreak_deuce_count_p = 0
  
  # probability of winning two points in a row after an infinite number of getting back to equal points
  while(tiebreak_ad_deuce_p^tiebreak_deuce_count > 0.00001){
    tiebreak_deuce_count_p = tiebreak_deuce_count_p + tiebreak_ad_deuce_p^tiebreak_deuce_count * pp^2
    tiebreak_deuce_count = tiebreak_deuce_count + 1
  }
  
  # overall probability of winning tiebreak = winning before 6-6 + winning after 6-6
  tiebreak_total_p = tiebreak_total_p + (tiebreak_deuce_p*tiebreak_deuce_count_p)
  
  total = total + tiebreak_p*tiebreak_total_p
  return(total)
}

p_match_bo3 = function(pp){
  ps = p_set(pp)
  
  total = 0
  
  # first 2 sets may be split, but third must be won by player X
  for (i in 0:1){
    total = total + choose(1+i, i)*ps^2*(1-ps)^i
  }
  
  return(total)
}

p_match_bo5 = function(pp){
  ps = p_set(pp)
  
  total = 0
  
  # first 4 sets may be split 2 to 2, but fifth must be won by player X
  for (i in 0:2){
    total = total + choose(2+i, i)*ps^3*(1-ps)^i
  }
  
  return(total)
}

nadal_p = 0.55

p_match_bo3(nadal_p)
p_match_bo5(nadal_p)
