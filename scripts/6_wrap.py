from openap import WRAP

wrap = WRAP(ac='A320')

def py_wrap():
  V = wrap.takeoff_speed()
  return V
