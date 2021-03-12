all:
	cabal v1-install packages/roshask/ packages/roshask/msgs/Geometry_msgs/ packages/roshask/msgs/Std_msgs/ packages/roshask/msgs/Kobuki_msgs/ packages/roshask/msgs/Turtlesim/ packages/roshask/msgs/Nav_msgs/ packages/roshask/msgs/Std_srvs/ packages/roshask/msgs/Actionlib_msgs/ packages/gloss-window rosy.cabal prepro/ rosy-base/ --force-reinstalls
    
rosy:
	cabal v1-install rosy.cabal rosy-base/ 
