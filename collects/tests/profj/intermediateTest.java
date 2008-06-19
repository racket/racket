// 14 checks; 2 failures
// 6 tests; no failures
// Order of calling testMethods crucial for test success

interface Automobile {
  int milesTraveled();
  void travel( int miles );
}

abstract class Auto implements Automobile { 
  int miles;
  int milesTraveled() { return miles; }
  
  void travel(int miles) {
    this.miles = this.miles + miles;
  }
}

class Car extends Auto {
  
  double basePrice;
  
  Car(int miles, double basePrice) {
    this.miles = miles;
    this.basePrice = basePrice;
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
  
  Car myCar = new Car(100000, 16000.00);
  Car momCar = new Car(10000, 32000.00);
  
  boolean test1 = check this.myCar expect this.momCar;
  boolean test2 = check this.myCar.milesTraveled() expect 100000;
  
  boolean testTravel() {
    myCar.travel(10);
    return (check this.myCar expect new Car(100010, 16000.00));
  }
  
  boolean testTravel2() {
    myCar.travel(10);
    return (check this.myCar expect new Car(100020, 16000.00));
  }
  
  boolean testPrice() {
    return (check this.myCar.price(2006) expect 16000.00 within .01) &&
    (check this.myCar.price(1991) expect 14933.33 within .01) &&
    (check this.myCar.price(2007) expect 32000.00 within .01);
  }
  
}

class Truck extends Auto {
  String make;
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
  
  String makeAndModel() {
    if (this.extendedBed) {
      return this.make.concat("Extended");
    } else {
      return this.make.concat(String.valueOf(this.numDoors));
    }
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
    oneTruck.travel(1000);
    return check this.oneTruck expect new Truck("Toyota",11000,2,false,20000.00);
  }
  
  boolean testMakeAndModel() {
    return (check this.oneTruck.makeAndModel() expect "Toyota2") &&
           (check this.twoTruck.makeAndModel() expect "FordExtended");
  }
  
}