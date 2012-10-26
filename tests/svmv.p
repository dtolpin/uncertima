problem svm {

  space (C, gamma) {
    C = [-8, -6 .. 8];
    gamma = [ -12, -9 .. 12 ];
  }

  observe (macc, vacc) {
    1=> (macc,vacc) = (0.04, 0.01) / 0.005 ;
  }
  
  find max util(C,gamma) = tanh(8*(macc/(1+vacc)-0.5));

  model lattice(C, gamma) {
    macc = (0.15, 1) / (0.1, 0.1) ;
    vacc = (0.1, 1) / (0.1, 0.1) ;
  }

}
