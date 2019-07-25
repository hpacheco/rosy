all:
	cabal install packages/roshask/ packages/roshask/msgs/Geometry_msgs/ packages/roshask/msgs/Std_msgs/ packages/roshask/msgs/Kobuki_msgs/ packages/roshask/msgs/Nav_msgs/ packages/roshask/msgs/Std_srvs/ packages/roshask/msgs/Actionlib_msgs/ packages/gloss-window --force-reinstalls
	cabal install
	cabal install prepro/ 
