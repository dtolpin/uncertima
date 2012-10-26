# $Id: obmark_manhattan.p,v 1.10 2009/08/16 09:52:51 dvd Exp $

# optimization benchmark with manhattan distance

problem obmark_manhattan {

  space (x, y)  / 0.02*(abs(@x)+abs(@y)) {
    x = [-1, -0.8 .. 1];
    y = [-1, -0.8 .. 1];
  }

  observe f {
    1=> f = 0.5 / 0.002;
  }
  
  find max util(x, y) = tanh(2*f);

  model lattice(x, y) {
    f = (0, 1) / (0.5, 0.5);
  }

}
