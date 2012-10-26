# $Id: obmark_free.p,v 1.7 2009/08/16 08:20:47 dvd Exp $

# optimization benchmark with free movements

problem obmark_free {

  space (x, y) {
    x = [-1.0, -0.8 .. 1.0];
    y = [-1.0, -0.8 .. 1.0];
  }

  observe f {
    1=> f = 0.5 / 0.01;
  }
  
  find max util(x, y) = tanh(2*f);

  model lattice(x, y) {
    f = (0, 1) / (0.5, 0.5);
  }

}
