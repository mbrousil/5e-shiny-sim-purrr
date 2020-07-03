
sim_damage_roll <- function(num_attacks, STR, PROF, sides, num_dice,
                            AC, crit, WEAP, crit_thresh) {
  
  # num_attacks = The number of attacks taken in a turn by the character
  # STR = Strength modifier
  # PROF = Proficiency modifier
  # sides = Num of sides of your damage dice, e.g. if roll 3d6 then 6 for d6
  # num_dice = Num of dice you roll on damage roll, e.g. if roll 3d6 then 3 for 3 dice
  # AC = AC of hypothetical opponent
  # crit = Num of dice you roll on critical hit damage roll,
  #   e.g. 4 if crit during sneak attack
  # WEAP = weapon bonus. i.e., if the weapon is a +1...then add +1 to your
  #   attack & damage rolls
  
  damage_records <- vector(mode = "numeric", length = num_attacks)
  
  for(i in 1:num_attacks){
    
    # Make an attack roll for one hand. Does the attack hit?
    attack_roll <- sum(sample(x = 1:20, size = 1, replace = T), STR, PROF, WEAP)
    
    # If it hits roll damage
    if(AC <= attack_roll & (attack_roll - (STR + PROF + WEAP)) < crit_thresh){
      
      # It hits, no crit
      damage_records[i] <-  sum(sample(x = 1:sides, size = num_dice, replace = T),
                                STR, WEAP)
      
      # If it doesn't hit, 0 damage
    }else if(attack_roll < AC){
      
      damage_records[i] <-  0
      
      # If it is a critical hit...
    }else if((attack_roll - (STR + PROF + WEAP)) >= crit_thresh){
      
      damage_records[i] <-  sum(sample(x = 1:sides, size = crit, replace = T),
                                STR, WEAP)
      
    }
    
  }
  
  # Return the total damage for all attacks in this run
  return(sum(damage_records))
}