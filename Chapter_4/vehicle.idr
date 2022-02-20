data PowerSource = Petrol | Pedal | Electricity

data Vehicle : PowerSource -> Type where
  Unicycle : Vehicle Pedal
  Bicycle : Vehicle Pedal
  Motocycle : (fuel : Nat) -> Vehicle Petrol
  Car : (fuel : Nat) -> Vehicle Petrol
  Bus : (fuel : Nat) -> Vehicle Petrol
  Tram : (ampers : Nat) -> Vehicle Electricity
  ElectricCar : (ampers : Nat) -> Vehicle Electricity

wheels : Vehicle power -> Nat
wheels Unicycle = 1
wheels Bicycle = 2
wheels (Motocycle _) = 2
wheels (Car fuel) = 4
wheels (Bus fuel) = 4
wheels (Tram ampers) = 6
wheels (ElectricCar ampers) = 4

refuel : Vehicle Petrol -> Vehicle Petrol
refuel (Car fuel) = Car 100
refuel (Bus fuel) = Bus 100
refuel (Motocycle fuel) = Motocycle 100

myBicycle : Vehicle Pedal
myBicycle = Bicycle
