
--------------------------------------------------------------------------------
-- | The rosy educational robot programming language.
--
-- You may use any of these functions and variables to control your robot.
module Rosy
    ( 
    -- $intro
    -- * Robot programming
      simulate, Robot(..), world1, world2, world3, world4
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
    -- ** Kobuki
    --
    -- *** Kobuki robot events (inputs)
    --
    {-| You can react to when the robot reports any of the following events.
    -}--
    -- **** Status
    --
    -- | When the robot is turned on or off.
    -- 
    , RobotStatus(..)
    --
    -- **** Buttons
    --
    -- | When someone presses or releases one of the three configurable buttons.
    -- 
    , Button0(..)
    , Button1(..)
    , Button2(..)
    , ButtonStatus(..)
    , Button(..)
    --
    -- **** Bumpers
    --
    -- | When the robot hits or steps away from a wall, as signaled by its three directional bumpers.
    , BumperLeft(..)
    , BumperCenter(..)
    , BumperRight(..)
    , BumperStatus(..)
    , Bumper(..), BumperSide(..)
    --
    -- **** Cliff sensors
    
    -- | When the robot is near or driving away from a hole in the ground, as signaled by its three directional cliff sensors.
    --
    , CliffLeft(..)
    , CliffCenter(..)
    , CliffRight(..)
    , CliffStatus(..)
    , Cliff(..), CliffSide(..)
    --
    -- **** Wheels
    --
    -- | When one of the two wheels of the robot gets stuck in a hole or escapes from a hole.
    --
    , WheelLeft(..)
    , WheelRight(..)
    , WheelStatus(..)
    , Wheel(..), WheelSide(..)
    --
    -- **** Periodic inputs
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
    -- *** Kobuki robot actions (outputs)
    --
    {-| You can order the robot to perform any of the following actions.
    -}
    --
    -- **** Sounds
    --
    -- | Play one of the pre-configured sounds.
    --
    , Sound(..)
    --
    -- **** Leds
    -- 
    -- | Change the color of one of the two robot led lights.
    --
    , Led1(..)
    , Led2(..)
    , LedColor(..)
    -- 
    -- **** Velocity
    --
    -- | Accelerate or deacelerate towards a desired velocity.
    --
    , Velocity(..)
    --
    {-|
    -}
    --
    -- ** Turtlesim
    --
    -- | Since you can control more than one turtle, Turtlesim events and actions are tagged with the specific turtle under control.
    --
    {-|
    -}
    --
    -- *** Turtles
    --
    -- | A fixed numbered turtle, from 1 to 9.
    --
    ,Turtle(..), IsTurtleNumber(..), TurtleNumber(..), turtleNumber
    --
    -- | One of the 9 available turtles
    --
    ,SomeTurtle(..), someTurtle, someTurtleNumber, onTurtle
    -- | Any of the 9 available turtles, used to listen to events and send actions to possibly different turtles.
    --
    ,AnyTurtle(..)
    --
    -- *** Periodic inputs
    --
    -- | Turtles periodically report information about their status.
    --
    -- * A turtle's current @Position@.
    --
    -- > Turtle n Position
    -- > AnyTurtle Position
    --
    -- * A turtle's current @Orientation@.
    --
    -- > Turtle n Orientation
    -- > AnyTurtle Orientation
    --
    -- * A turtle's current @Velocity@.
    --
    -- > Turtle n Velocity
    -- > AnyTurtle Velocity
    --
    {-|
    -}
    --
    -- *** Actions
    --
    -- | You can order a turtle to perform any of the following actions.
    --
    -- * Update a turtle's @Velocity@.
    --
    -- > Turtle n Velocity
    -- > AnyTurtle Velocity
    --
    {-|
    -}
    --
    -- *** Parameters
    --
    -- | Turtlesim sets and listens to the following global parameters.
    --
    -- **** Background
    --
    -- | Read or change the background color. Note that after a change, Turtlesim will only update background color with the execution of certain tasks.
    --
    , Background(..), Color(..)
    --
    {-|
    -}
    --
    -- ** General events and actions
    --
    {-| Events and actions that an be used for any robot
    -}
    --
    -- *** Time
    --
    -- | Get the current time (input
    --
    , module Rosy.Controller.Time
    --
    -- *** Randomness
    --
    -- | Generate a random number (input)
    -- 
    , StdGen(..)
    --
    -- *** Speak 
    --
    -- | Say some sentence (output).
    --
    , Say(..)
    --
    -- *** Never 
    --
    -- | An event that never occurs (input or output)
    --
    , Never(..)
    --
    -- *** User-defined events
    --
    {-| You can define new input or output events seamlessly by declaring new data types.
    -}
    --
    -- *** User-defined memory
    -- 
    {-| You can give your controller memory (i.e. state) by declaring inputs and outputs with the additional 'Memory' type tag.
    -}
    --
    -- | A type declaration for processing controller memory.
    ,Memory(..)
    --
    -- *** User-defined global parameters
    -- 
    {-| You can share information among multiple controllers using global parameters by declaring inputs and outputs with the additional 'Param' type tag.
    -}
    --
    -- | A type declaration for processing global parameters.
    ,Param(..)
    --
    -- ** Tasks
    --
    -- | You can also control your robot by sequencing tasks.
    --
    , task, subTask, Done(..), noInit
    , Task --,DoneT
    , call, Call, Cancel(..), noCancel, Feedback(..), noFeedback,say
    , parallel, parallel_, race, race_
    --
    -- | You can compose and create new tasks to better control your robot simulations.
--    , module Control.Effect
    , module Control.Monad
    --
    -- *** Turtlesim
    --
    -- | You can control the Turtlesim simulator via the following tasks.
    --
    -- * clear the background canvas and update the background color.
    , clear
    --
    -- |
    --
    -- * reset the whole simulation and update the background color.
    , reset
    --
    -- |
    --
    -- * kill a turtle by number.
    , kill
    --
    -- |
    --
    -- * spawn a new turtle at a given 'Position' and with a given 'Orientation'; the numbber of the created turtle is returned.
    , spawn
    --
    -- |
    --
    -- * changes the 'Pen' for a particular turtle.
    , setPen, Pen(..), OnOff(..)
    --
    -- |
    --
    -- * teleports a particular turtle to a new 'Position' and 'Orientation'.
    , teleportAbsolute
    --
    -- |
    --
    -- * teleports a particular turtle by a given 'Distance'.
    , teleportRelative
    --
    -- * Utilities
    -- ** Geometry functions.
    , Degrees(..)
    , degreesToOrientation, orientationToDegrees, normOrientation
    , Centimeters(..), Meters(..)
    , centimetersToMeters, metersToCentimeters
    , vecToPosition, positionToVec
    , module Rosy.Util
    -- ** General-purpose functions.
    , module Prelude 
    , module System.Random
    , module Data.Time.Clock
    
    ) where

--import Control.Effect
import Control.Monad --hiding ((>>),(>>=),(>>=),return,fail)
import Rosy.Controller.Time
import Rosy.Controller.Core
import Rosy.Controller.Kobuki
import Rosy.Controller.Turtlesim
import Rosy.Robot.Kobuki.Core
import Rosy.Robot.Kobuki.State
import Rosy.Viewer.Kobuki.Core
import Rosy.Viewer.Kobuki.State 
import Rosy.Interface
import Rosy.Interface.Task
import Rosy.Interface.Task.Types
import Rosy.Util
import Prelude hiding ((>>),(>>=),return,fail)
import Data.Time.Clock
import System.Random

--------------------------------------------------------------------------------
-- $intro
-- Welcome to Rosy! You can program your own robot by defining variables and functions.
-- To create a Rosy program you'll define the variable called @main@ using 'simulate'. The first parameter is the kind of robot you want to control. The second parameter to 'simulate' should be a function that does the actual job of controlling your robot.
--
-- Start by trying out the smallest Rosy program. You can experiment with two familiar robots. A Kobuki robot equipped with a couple of different sensor:
--
-- > main = simulate (Kobuki Nothing) ()
-- Or a very simple turtle that leaves a trail on the map:
--
-- > main = simulate Turtlesim ()
-- We are not telling the robot to do anything, and so it will not surprisingly stand still. Not very interesting...
--
-- What about actually moving our robot? We can set its desired velocity:
--
-- > main = simulate (Kobuki Nothing) (Velocity 1 0)
-- This time, the robot will move forward at a constant velocity of /1m\/s/.
--
-- We can do the same for a turtle. Since we can possibly create and control more than one turtle, we need to explicitely refer to the current first turtle:
--
-- > move :: Turtle 1 Velocity
-- > main = simulate Turtlesim (Turtle (Velocity 1 0))
-- This time, the turtle moves forward and leaves a trail along its path.
--
-- Ok, but how can we vary the velocity of the robot, for example, to make it accelerate? We need to know the previous velocity, and increase it:
--
-- > accelerate :: Velocity -> Velocity
-- > accelerate (Velocity linear angular) = Velocity (linear+1) angular
-- >
-- > main = simulate (Kobuki Nothing) accelerate
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
-- > main = simulate (Kobuki Nothing) (accelerate,warningWall)
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
-- > main = simulate (Kobuki Nothing) (accelerate,warningWall,playError)
--
-- And you can see the drill. More intesting programs will likely combine several controllers, react to multiple events, or issue multiple commands.
--
-- Go explore, use your imagination! 


