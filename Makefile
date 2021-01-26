all:
	cabal install packages/roshask/ packages/roshask/msgs/Geometry_msgs/ packages/roshask/msgs/Std_msgs/ packages/roshask/msgs/Kobuki_msgs/ packages/roshask/msgs/Turtlesim/ packages/roshask/msgs/Nav_msgs/ packages/roshask/msgs/Std_srvs/ packages/roshask/msgs/Actionlib_msgs/ packages/gloss-window rosy.cabal prepro/ ../codeworld-rosy/rosy-base --force-reinstalls
    
rosy:
	cabal install rosy.cabal ../codeworld-rosy/rosy-base 
