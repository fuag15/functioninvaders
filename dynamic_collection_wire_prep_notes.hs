{-
  Exploration of events to continouse functions using frp
-}

{-# Language TupleSections #-}
module Frpgun where

{-
  Event transformations with the end goal of on a rate limited trigger event, spawn a Bullet behavior at that time. Eventually want to form this in Netwire 5.
-}

type Position   = Double            -- ^ For now assume we are in 1d space and position is along a vector and the space is bounded below by 0
type Velocity   = Double            -- ^ Velocity in a 1d space is a Double in this simulation
type Time       = Double            -- ^ Time is represented as a Double for our purpose
type Event    a = [(Time, a)]       -- ^ An event is a list of occurrences matched with a time
type Behavior a = Time -> a         -- ^ A behavior is a continuous time function that outputs the value of that behavior at any given time
type Bullet     = Behavior Position -- ^ A bullet is described completely by its current position in this simulation
type Gun        = Behavior [Bullet] -- ^ A gun behavior describes the position of any bullets it fired

main :: IO ()
main = print shotFired

-- | Base trigger Pulled Event stream
triggerPulled :: Event () -- ^ The event of a trigger event has no meaningful value associated with it
triggerPulled = [ (2 , ())
                , (5 , ())
                , (6 , ())
                , (11, ()) ]

-- | Generates a behavior that is fully represented by that bullets position
generateBullet :: Position          -- ^ Starting position for the bullet
               -> Velocity          -- ^ Velocity at which this bullet travels
               -> Behavior Position -- ^ The bullets position at any time
generateBullet initial_position velocity time = initial_position + velocity * time

-- | Accumulator to rate limit events
eventRateFilter :: Time                            -- ^ Time to limit the rate of events
                -> Event a -> (Time, a) -> Event a -- ^ Accumulator to fold over events
eventRateFilter target_rate rate_limited_events next_event@(next_event_occurrence, _) =
    let (last_event_occurrence, _) = last rate_limited_events
        event_difference           = next_event_occurrence - last_event_occurrence
    in
    if event_difference < target_rate
      then rate_limited_events
      else rate_limited_events ++ [next_event]

-- | One event per shot fired, with a 3 second reload time
shotFired :: Event () -- ^ stream of rate limited shots
shotFired = foldl (eventRateFilter 3) [head triggerPulled] $ tail triggerPulled

-- | represent an event that never happens
never :: Event a -- ^ Coerce this list into any type of event
never = []

-- | represent an event that happens immediately
rightNow :: a       -- ^ base value for event
         -> Event a -- ^ an event stream of with that value that happens immidiately
rightNow value = [(0, value)]

-- | represent an event that happens at a specific point in time
at :: Time    -- ^ time at which event occurs
   -> a       -- ^ value for event
   -> Event a -- ^ an event stream of one event that happens at the given time
at time value = [(time, value)]

-- | represent a periodic behavior
periodically :: Time    -- ^ time interval at which to make events happen
             -> a       -- ^ value for interval
             -> Event a -- ^ Stream of events that are of th egiven value at the given times
periodically time_delta value = map (, value) event_times
  where
    event_times = map (time_delta *) [1..]

-- | Switch existing behavior to a behavior in an event
-- switch :: Event (Behavior a) -> Behavior a -> Behavior a

-- | Generate a gun behavior, a gun generates bullets over time
generateGun :: Position          -- ^ position of the gun
            -> Velocity          -- ^ Velocity of bullets that the gun fires
            -> Behavior [Bullet] -- ^ A gun produces bullets over time
generateGun position bullet_velocities = const [] -- ^ Placeholder, no clue how to form this best yet

{-
  follow up exploration implimenting a continouse time switch like behavior
-}

switch :: Event (Behavior a) -- ^ Event to switch on
       -> Behavior a         -- ^ initial behavior
       -> Behavior a         -- ^ This switch produces a behavior
switch []                        initial_behavior time = initial_behavior time
switch (next_event:other_events) initial_behavior time =
  let next_event_time           = fst next_event
      shift_event (time', event) = (time' - next_event_time, event)
      shifted_events            = fmap shift_event other_events
  in
  if time < next_event_time
    then initial_behavior time
    else switch shifted_events (snd next_event) $ time - next_event_time

-- | Generate a gun behavior, a gun generates bullets over time
generateGun :: Position          -- ^ position of the gun
            -> Velocity          -- ^ Velocity of bullets that the gun fires
            -> Behavior [Bullet] -- ^ A gun produces bullets over time
generateGun _ _ = const [] -- ^ Placeholder

{-
  Follow up exploration on crafting an actual collection wire

  make a dynamic version of modes that incorperates multicast with
  an event for adding a wire and a way for a wire to say it died

  > MyWire (a, Event (MyCollOp a b)) b -- my collop is a transform that lest you add adn delete wires ~ addressed by a key

  this is relevant in v4
  http://hackage.haskell.org/package/netwire-4.0.7/docs/Control-Wire-Trans-Combine.html
  and the multicast wire
  add an Event to control the collection
  individual wires must either be intervals or must return an event to tell the multicaster that they want to die
  add an event to add new wires to the collection

  look into alt, plus, alternative / traversable, see semigroupoids -- gives use of map

  > --~ construct this out out of modes and multicast
  > collection :: (Traversable f) => f (MyWire a b) -> MyWire (a, Event (f (MyWire a b) -> f (MyWire a b))) b
-}


