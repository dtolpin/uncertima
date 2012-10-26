problem wafer {

  space (sitex, sitey, focus, color, angle) /
    0.01*(nz(abs(@sitex)+abs(@sitey)+abs(@focus))+nz(@color)+3*nz(@angle)) {
    sitex = [-1 .. 1];
    sitey = [-1 .. 1];
    focus = [1 .. 31];
    color = [1 .. 5];
    angle = [0, 180];
  }

  observe (x, y, px, py) {
    1 => (x, y, px, py) = (1.0, 1.0, 1.0, 1.0) / 0.01;
  }
  
  find max merit(focus, color) =
    0.5-0.083*(tanh(3*(xprec-1))+tanh(3*(yprec-1))
               +tanh(1.5*(xtis-2))+tanh(1.5*(ytis-2))
               +tanh(1.5*(xtsv-2))+tanh(1.5*(ytsv-2))) {
    xprec = mean(px);
    yprec = mean(py);
    xtis = abs(mean(x[angle=180]+x[angle=0]));
    ytis = abs(mean(y[angle=180]+y[angle=0]));
    xtsv = sd(x[angle=180]+x[angle=0]);
    ytsv = sd(y[angle=180]+y[angle=0]);
  }

  model lattice(focus, sitex, sitey) {
    x = (1.0, 100.0) / (4.0, 16.0, 16.0);
    y = (1.0, 100.0) / (4.0, 16.0, 16.0);
    px = (1.0, 100.0) / (4.0, 16.0, 16.0);
    py = (1.0, 100.0) / (4.0, 16.0, 16.0);
  }
}
