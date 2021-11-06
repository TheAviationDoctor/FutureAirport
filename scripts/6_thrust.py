from openap import Thrust

thrust = Thrust(ac='A320', eng='CFM56-5B4')

def py_thrust(tas, alt):
  T = thrust.takeoff(tas = tas, alt = alt)
  return T
