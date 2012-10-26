problem sample {

  space (x, y, z, t)            # four coordinates
        / 0.005*@x+0.02*step(@y)+0.04*t { # linear on x, fixed on y, free on z, and the time ticks 
    # each one is specified by range of values and by change const 
    x = [0.0, 10.0 .. 100.0];
    y = [0.0, 5.0 .. 100.0]; 
    z = [0, 1];
    t = [1..20]; 
  }

  observe (a, b) {               # two features observed in every point
    # two observations
    1 => (a,b) = (10.0, 10.0) / 0.01; # both features, imprecise, cheap 
    2 => a = 2.0 / 0.1;                 # only a, accurate, expensive
  }
  
  find max w(x,y) = log(u)+log(v) { # optimize x,y for maximum of w
    u = mean(a[z=1]-a[z=0]);
    v = sd(b); 
  }

  model lattice(x,y,t)  { # the dependency model is lattice
    # with dependencies on x, y, and t
    a = (0.0, 10.0) / (5, 5, 5);
    b = (0.0, 10.0) / (10, 10, 2);
  }

  # another options would be 
  # model bif "sample-model.bif";
  # model gdl "sample-model.gdl";
}
