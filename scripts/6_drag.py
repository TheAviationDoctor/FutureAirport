from openap import Drag

drag = Drag(ac='A320')

D = drag.nonclean(mass=60000, tas=150, alt=100, flap_angle=20,
path_angle=10, landing_gear=True)


def py_drag(mass, tas, alt, flap_angle, path_angle, landing_gear):
  D = drag.nonclean(mass = mass, tas = tas, alt = alt, flap_angle = flap_angle, path_angle = path_angle, landing_gear = landing_gear)
  return cd0_total
