# $Id: obmark_free.p,v 1.2 2009/11/30 21:23:49 dvd Exp $

# optimization benchmark with free movements

problem obmark_free {

  space (x, y) {
    x = [-0.8, -0.6 .. 0.8];
    y = [-0.8, -0.6 .. 0.8];
  }

  observe f {
    1=> f = 0.5 / 0.01;
  }
  
  find max util(x, y) = tanh(2*f);

  model lattice(x, y) {
    f = (0, 1) / (0.5, 0.5);
  }

}
