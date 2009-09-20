// Expected results:
//  14 checks
//  2 failed checks, one in each test class
//  6 tests, all passing
//  All methods of both classes are covered

interface Automobile {
  int milesTraveled();
  Automobile travel( int miles );
  String makeAndModel();
  double price(int year);
}

class Car implements Automobile {
  
  String make;
  String model;
  int miles;
  double basePrice;
  
  Car(String make, String model, int miles, double basePrice) {
    this.make = make;
    this.model = model;
    this.miles = miles;
    this.basePrice = basePrice;
  }
  
  int milesTraveled() {
    return this.miles;
  }
  String makeAndModel() {
    return this.make.concat(this.model);
  }
  
  Automobile travel(int miles) {
    return new Car(this.make, this.model, this.miles+miles, this.basePrice);
  }
  
  double price(int year) {
    if ((2006 - year) == 0) {
      return this.basePrice;
    } else {
      if ((2006 - year) > 0) {
        return this.basePrice - (this.basePrice / (2006 - year));
      } else {
        return this.basePrice + (this.basePrice / (year - 2006));
      }
    }
  }
  
}

class CarExamples {
  
  CarExamples() { }
  
  Car myCar = new Car("Toyota","Tercel",100000, 16000.00);
  Car momCar = new Car("Honda","Excel",10000, 32000.00);
  
  boolean test1 = check this.myCar expect this.momCar;
  boolean test2 = check this.myCar.milesTraveled() expect 100000;
  
  boolean testTravel() {
    return (check this.myCar.travel(10) expect new Car("Toyota","Tercel",100010, 16000.00)) ||
    (check this.momCar.travel(90000) expect this.myCar);
  }
  
  boolean testMakeModel() {
    return check this.myCar.makeAndModel() expect "ToyotaTercel";
  }
  
  boolean testPrice() {
    return (check this.myCar.price(2006) expect 16000.00 within .01) &&
    (check this.myCar.price(1991) expect 14933.33 within .01) &&
    (check this.myCar.price(2007) expect 32000.00 within .01);
  }
  
}

class Truck implements Automobile {
  String make;
  int miles;
  int numDoors;
  boolean extendedBed;
  double basePrice;
  
  Truck( String make, int miles, int numDoors, boolean bed, double basePrice) {
    this.make = make;
    this.miles = miles;
    this.numDoors = numDoors;
    this.extendedBed = bed;
    this.basePrice = basePrice;
  }
  
  int milesTraveled() { return this.miles; }
  String makeAndModel() {
    if (this.extendedBed) {
      return this.make.concat("Extended");
    } else {
      return this.make.concat(String.valueOf(this.numDoors));
    }
  }
  Automobile travel(int miles) {
    return new Truck(this.make, this.miles + miles, this.numDoors, this.extendedBed, this.basePrice);
  }
  double price( int year ) {
    // Uncomment to test runtime error behavior
    //return this.basePrice - (2 * (this.basePrice / (2006 -year)));
    if (year == 2006) {
      return this.basePrice;
    } else {
      return this.basePrice - (2 * (this.basePrice / (2006 - year)));
    }
  }
  
}

class TruckExamples {
  Truck oneTruck = new Truck("Toyota",10000, 2,false,20000.00);
  Truck twoTruck = new Truck("Ford",100000,2,true,35000.00);
  
  boolean test1 = check this.oneTruck.milesTraveled() expect 10000;
  boolean test2 = check this.oneTruck expect this.twoTruck;
  
  TruckExamples() { }
  
  boolean testPrice() {
    return (check this.oneTruck.price(2006) expect 20000.00 within .01) &&
           (check this.oneTruck.price(1996) expect 16000.00 within .01);
  }
  
  boolean testTravel() {
    return check this.oneTruck.travel(1000) expect new Truck("Toyota",11000,2,false,20000.00);
  }
  
  boolean testMakeAndModel() {
    return (check this.oneTruck.makeAndModel() expect "Toyota2") &&
           (check this.twoTruck.makeAndModel() expect "FordExtended");
  }
  
}

