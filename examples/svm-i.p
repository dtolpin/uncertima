problem svm {

  space (cost, gamma) {
    cost = [1, 3 .. 21];
    gamma = [1, 3 .. 21];
  }

  observe accuracy {
    1=> accuracy = 0.25 / 0.01;
  }
  
  find max util(cost, gamma) = tanh(4*(accuracy-0.5));

  model lattice(cost, gamma) {
    accuracy = (0.3, 1.0) / (0.4, 0.4);
  }
}
