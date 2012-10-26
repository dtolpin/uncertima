problem svm {

  space (C, gamma) {
    C = [-8, -6 .. 8];
    gamma = [ -12, -9 .. 12 ];
  }

  observe acc {
    1=> acc = 0.04 / 0.01;
  }
  
  find max util(C,gamma) = tanh(5*(acc-0.5));

  model lattice(C, gamma) {
    acc = (0.15, 1.0) / (0.1, 0.1);
  }

}
