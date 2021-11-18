from openap import Drag

drag = Drag(ac='A320')



def py_drag(mass, tas, alt, flap_angle, path_angle, landing_gear):
  D = drag.nonclean(mass = mass, tas = tas, alt = alt, flap_angle = flap_angle, path_angle = path_angle, landing_gear = landing_gear)
  return D



def py_dragpolar(self):
  dragpolar = dragpolar(self = self)
  return dragpolar
