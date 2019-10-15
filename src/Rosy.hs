
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
    -}--
    -- *** Status
    --
    -- | When the robot is turned on or off.
    -- 
    , RobotStatus(..)
    --
    -- *** Buttons
    --
    -- | When someone presses or releases one of the three configurable buttons.
    -- 
    , Button0(..)
    , Button1(..)
    , Button2(..)
    , ButtonStatus(..)
    , Button(..)
    --
    -- *** Bumpers
    --
    -- | When the robot hits or steps away from a wall, as signaled by its three directional bumpers.
    , BumperLeft(..)
    , BumperCenter(..)
    , BumperRight(..)
    , BumperStatus(..)
    , Bumper(..)
    --
    -- *** Cliff sensors
    
    -- | When the robot is near or driving away from a hole in the ground, as signaled by its three directional cliff sensors.
    --
    , CliffLeft(..)
    , CliffCenter(..)
    , CliffRight(..)
    , CliffStatus(..)
    , Cliff(..)
    --
    -- *** Wheels
    --
    -- | When one of the two wheels of the robot gets stuck in a hole or escapes from a hole.
    --
    , WheelLeft(..)
    , WheelRight(..)
    , WheelStatus(..)
    , Wheel(..)
    --
    -- *** Periodic inputs
    --
    -- | The robot periodically reports additional information about its status.
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
    , module Rosy.Controller.Time
    --
    -- |
    --
    -- * A randomness generator
    , StdGen(..)
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
    , LedColor(..)
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
    -- ** User-defined events
    --
    {-| You can define new input or output events seamlessly by declaring new data types.
    -}
    --
    -- ** User-defined memory
    -- 
    {-| You can give your controller memory (i.e. global state) by declaring inputs and outputs with the additional 'Memory' type tag.
    -}
    --
    -- | A type declarating for processing global memory.
    ,Memory(..)
    --
    -- * Utilities
    -- ** Geometry functions.
    , radiansToDegrees
    , degreesToRadians
    -- ** General-purpose functions.
    , floorFloating
    , roundFloating
    , ceilingFloating
    , module Prelude
    , module System.Random
    , module Data.Time.Clock
    ,
    ) where

import Rosy.Controller.Time
import Rosy.Controller.Core
import Rosy.Controller.Kobuki
import Rosy.Robot.Kobuki
import Rosy.Robot.State
import Rosy.Viewer.Core
import Rosy.Viewer.State 
import Rosy.Interface
import Rosy.Util
import Prelude
import Data.Time.Clock
import System.Random

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
-- Thinking further, what if our robot hits a wall? It would be nice if our controller at least played a sound to signal that it has crashed. But how does the controller know when to react? Luckily, our robot comes equipped with a front bumper that will be pressed on contact. We can simply write a @warningWall@ controller that will wait for the bumper to be pressed, and when it happens tell the robot to produce an error sound. To ignore when the bumper is released, we can use a 'Maybe':
--
-- > warningWall :: BumperCenter -> Maybe Sound
-- > warningWall (BumperCenter Pressed)  = Just ErrorSound
-- > warningWall (BumperCenter Released) = Nothing
--
-- Now we can just install both controllers:
--
-- >
-- > main = simulate (accelerate,warningWall)
--
-- Running this example, the robot will play a sound exactly once when it hits a wall. But what if we wanted to play an error sound forever? To perform something like that, we need to give our controller memory: it normally processes each eevnt independently, but will now need to remember that it has hit a wall before. In Rosy, you can give your controller memory by defining a new data type:
--
-- > data Hit = NoHit | NoHit
-- > warningWall :: BumperCenter -> Maybe (Memory Hit)
-- > warningWall (BumperCenter Pressed)  = Just (Memory Hit)
-- > warningWall (BumperCenter Released) = Nothing
-- >
-- > playError :: Memory Hit -> Maybe Sound
-- > playError (Memory Hit) = Just ErrorSound
-- > playError (Memory NoHit) = Nothing
-- > 
-- > main = simulate (accelerate,warningWall,playError)
--
-- And you can see the drill. More intesting programs will likely combine several controllers, react to multiple events, or issue multiple commands.
--
-- Go explore, use your imagination! 


