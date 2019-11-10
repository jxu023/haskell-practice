module Rules where 

import Model

toMove :: Turn -> Player
toMove t = case mod t 4 of 0 -> Red
                           1 -> Blue
                           2 -> Blue
                           3 -> Red


