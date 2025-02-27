13+20+22+28+30+36+35+39+49+39+39+23+32+23+17+13 # = 458
20+24+26+26+29+36+34+28+29+23+22+43+39+24+24+14 # = 441

458/16 # = 28.625
441/16 # = 27.5625

# 20 outlined regions

458/20 # = 22.9 so x >= 23

# 1-shape is size 5
# 2-shape is size 5 * 2*2*2 = 40
# 3-shape is size 5 * 3*3*3 = 135
# 4-shape is size 5 * 4*4*4 = 320 (not possible - see below)
# 5-shape is size 5 * 5*5*5 = 625 (not possible for same reason)

# But, for a 4-shape, there need to be 4 rows or columns of size > 12*4 = 48
# which there aren't... so max size shape is 3

# Max shape size is 3 so max strip is 17*3 = 51, so total <= 441+51=492

# smallest shape size is 8 so x <= 8*3 = 24

# so 23 <= x <= 24

# so total is either 23*20=460 or 24*20=480
# BUT, if it is 460, then the missing row has to be 2 (460-458), but there are two squares
# in a shape of size 8, so those must both contain either 2 or 3, so this 
# isn't possible

# So x = 24!!!

# Therefore, bottom number is 480-458=22
# Side number is 480-441=39

# NOT PROVEN but looks like The top shape has to be in 1 pos only. 
# And bottom in 1 pos only. No further space for any 3-shapes anywhere if so
# But this looks likely from the row/col nums. Must be a 2-shape to make 49 in
# middle col



### Simple version

9+12+12+10+8+8+2 # = 61
2+7+8+13+10+9+3 # = 52

61/7 # = 8.714
52/7 # = 7.4286

# 13 outlined regions

61/13 # = 4.69 therefore x must be >= 5 where x is sum inside each outlined region
# Max is 6 because smallest shape is size 3 and largest shape could be x2

# So x is either 5 or 6

# if 5, then 13 * 5 = 65, bottom is 4, side is 13
# if 6, then 13 * 6 = 78, bottom is 17, side is 26 (not possible)