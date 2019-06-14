
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
    
       (1) whose __inputs__ are events that occured in the robot to which we want to __react__;
       2. whose __outputs__ are commands that we __order__ the robot to perform.
    -}
    -- | In other words, a controller is a function that we install on the robot. It reads events from the robot and issues commands back to the robot.
    --
    -- The type of a controller therefore establishes how it interacts with the robot. Writing controllers as simply as we write functions is very powerful:
    --
    -- * A controller can listen to more than one robot event at the same time:
    --
    -- > controller :: event1 -> event2 -> command
    --
    -- * or produce more than one robot command at the same time:
    --
    -- > controller :: event -> (command1,command2)
    --
    -- * A controller that may or not produce a command can be written using a 'Maybe':
    --
    -- > controller :: event -> Maybe command
    --
    -- * A controller that produces one of multiple alternative robot commands can be written as an 'Either':
    --
    -- > controller :: event -> Either command1 command2
    --
    -- * It is often useful to install multiple simultaneous controllers on the robot. To do so, we just need to create a tuple of controllers:
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
    -- | When the robot hits of steps away from a wall, as signaled by its three directional bumpers.
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
    -- ** Robot commands (outputs)
    --
    {-| You can command the robot to do any of the following actions.
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
-- We are not telling the robot to do anything in particular, and so it will not surprisingly stand still. Not very interesting...
--
-- What about actually moving our robot? We can move our robot forward by setting its desired velocity.
--
-- > main = simulate (Velocity 1 0)
-- This time, the robot will move forward at a constant velocity of /1m\/s/.
--
-- Ok, but can we vary the velocity of the robot, for example, to make it accelerate at a rate of /1m\/s^2/? Sure, but our controller now needs to become a function, as it needs to know the previous velocity to increase it:
--
-- > accelerate :: Velocity -> Velocity
-- > accelerate (Velocity linear angular) = Velocity (linear+1) angular
-- >
-- > main = simulate accelerate
--
-- Indeed, the robot will acelerate at a rate of /1m\/s^2/. This is because the @acelerate@ controller is updating the velocity every second.
--
-- Thinking further, what if our robot hits a wall? It would be nice if our controller at least produced a warning sound. But how does the controller know when to react? Luckily, our robot comes equipped with a front bumper that will be pressed on contact. Therefore, we can write a @warning@ controller that will wait for the bumper to be pressed, and tell the robot to produce an error sound:
--
-- > warning :: BumperCenter -> Sound
-- > warning (BumperCenter Pressed) = ErrorSound
--
-- Now we can just install both controllers together:
--
-- >
-- > main = simulate (accelerate,warning)
--
-- And you can see the drill. More intesting programs will likely combine several controllers, react to multiple events, or issue multiple commands.
--
-- Go explore, use your imagination! 


