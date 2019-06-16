
--------------------------------------------------------------------------------
-- | The rosy educational robot programming language.
--
-- You may use any of these functions and variables to control your robot.
module Rosy
    ( 
    -- $intro
    -- * Robot programming
      simulate
    -- * Robot controllers
    --
    {-| A robot 'Controller' is a function:
    
       (1) whose __inputs__ are events that occured in the robot to which we want to __react__ to;
       2. whose __outputs__ are actions that we __order__ the robot to perform.
    -}
    -- | In other words, a controller is a piece of code that we install on the robot to monitor its events and order it to act accordingly.
    --
    -- The first part to designing a controller is to establish how it interacts with the robot:
    --
    -- * A controller can react to more than one robot event at the same time:
    --
    -- > controller :: event1 -> event2 -> action
    --
    -- * or trigger more than one robot action at the same time:
    --
    -- > controller :: event -> (action1,action2)
    --
    -- * Depending on the situation, a controller may or not produce an action:
    --
    -- > controller :: event -> Maybe command
    --
    -- * or produce one out of multiple actions:
    --
    -- > controller :: event -> Either action1 action2
    --
    -- * It is often useful to install multiple controllers on the robot:
    --
    -- > (controller1,controller2)
    --
    {-
    -}
    -- ** Robot events (inputs)
    --
    {-| You can react to when the robot reports any of the following events.
    -}
    --
    -- *** Buttons
    --
    -- | When someone presses or releases one of the three configurable buttons.
    -- 
    , Button0(..)
    , Button1(..)
    , Button2(..)
    --
    -- *** Bumpers
    --
    -- | When the robot hits or steps away from a wall, as signaled by its three directional bumpers.
    , BumperLeft(..)
    , BumperCenter(..)
    , BumperRight(..)
    --
    -- *** Cliff sensors
    
    -- | When the robot is near or driving away from a hole in the ground, as signaled by its three directional cliff sensors.
    --
    , CliffLeft(..)
    , CliffCenter(..)
    , CliffRight(..)
    --
    -- *** Wheels
    --
    -- | When one of the two wheels of the robot gets stuck in a hole or escapes from a hole.
    --
    , WheelLeft(..)
    , WheelRight(..)
    --
    -- *** Periodic inputs
    --
    -- | The robot periodically (every second) reports additional information about its status.
    --
    -- * Its current position.
    , Position(..)
    --
    -- | 
    --
    -- * Its current orientation.
    , Orientation(..)
    --
    -- |
    --
    -- * Its current velocity.
    , Velocity(..)
    --
    -- |
    --
    -- * The current time.
    , Clock(..)
    --
    -- ** Robot actions (outputs)
    --
    {-| You can order the robot to perform any of the following actions.
    -}
    --
    -- *** Sounds
    --
    -- | Play one of the pre-configured sounds.
    --
    , Sound(..)
    --
    -- *** Leds
    -- 
    -- | Change the color of one of the two robot led lights.
    --
    , Led1(..)
    , Led2(..)
    -- 
    -- *** Velocity
    --
    -- | Accelerate or deacelerate towards a desired velocity.
    --
    , Velocity(..)
    --
    -- *** Speak
    --
    -- | Say some sentence.
    --
    , Say(..)
    --
    -- * Utilities
    -- ** Geometry functions.
    , radiansToDegrees
    , degreesToRadians
    -- ** General-purpose functions.
    , module Prelude
    ) where

import Rosy.Controller.Core
import Rosy.Controller.Kobuki
import Rosy.Robot.Kobuki
import Rosy.Robot.State
import Rosy.Viewer.Core
import Rosy.Viewer.State 
import Rosy.Interface
import Rosy.Util
import Prelude

--------------------------------------------------------------------------------
-- $intro
-- Welcome to Rosy! You can program your own robot by defining variables and functions.
-- To create a Rosy program you'll define the variable called @main@ using 'simulate'. The parameter to 'simulate' should be a function that does the actual job of controlling your robot.
--
-- Start by trying out the smallest Rosy program:
--
-- > main = simulate ()
-- We are not telling the robot to do anything, and so it will not surprisingly stand still. Not very interesting...
--
-- What about actually moving our robot? We can set its desired velocity:
--
-- > main = simulate (Velocity 1 0)
-- This time, the robot will move forward at a constant velocity of /1m\/s/.
--
-- Ok, but how can we vary the velocity of the robot, for example, to make it accelerate? We need to know the previous velocity, and increase it:
--
-- > accelerate :: Velocity -> Velocity
-- > accelerate (Velocity linear angular) = Velocity (linear+1) angular
-- >
-- > main = simulate accelerate
--
-- If you try this out, the robot will indeed acelerate forward.
--
-- Thinking further, what if our robot hits a wall? It would be nice if our controller at least played a sound to signal that it has crashed. But how does the controller know when to react? Luckily, our robot comes equipped with a front bumper that will be pressed on contact. We can simply write a @warningWall@ controller that will wait for the bumper to be pressed, and when it happens tell the robot to produce an error sound:
--
-- > warningWall :: BumperCenter -> Sound
-- > warningWall (BumperCenter Pressed) = ErrorSound
--
-- Now we can just install both controllers:
--
-- >
-- > main = simulate (accelerate,warningWall)


-- React to events when they happen
--
-- > ok :: Maybe BumperCenter -> Maybe Led1
-- > ok (Just (BumperCenter Pressed)) = Just (Led1 Red)
-- > ok (Just (BumperCenter Release)) = Just (Led1 Green)
-- > ok Nothing = 
--
-- And you can see the drill. More intesting programs will likely combine several controllers, react to multiple events, or issue multiple commands.
--
-- Go explore, use your imagination! 


