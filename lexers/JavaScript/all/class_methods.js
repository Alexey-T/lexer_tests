// Parent class: Vehicle
class Vehicle {
    constructor(brand, year) {
        this.brand = brand;
        this.year = year;
    }

    // Method with nested functions
    getInfo() {
        function formatBrand(brand) {
            return brand.toUpperCase();
        }

        function calculateAge(currentYear) {
            return currentYear - this.year;
        }

        const age = calculateAge(2026); // assuming current year is 2026
        console.log(`Vehicle Info:`);
        console.log(`  Brand: ${formatBrand(this.brand)}`);
        console.log(`  Year: ${this.year}`);
        console.log(`  Age: ${age} years old`);
        
        return `${formatBrand(this.brand)} (${this.year})`;
    }

    // Another method with a nested helper
    startEngine() {
        function playSound() {
            return "Vroom vroom!";
        }

        function checkFuel() {
            return Math.random() > 0.1 ? "sufficient" : "low";
        }

        const fuelStatus = checkFuel();
        console.log(`Starting ${this.brand}'s engine...`);
        console.log(`Fuel level: ${fuelStatus}`);
        console.log(playSound());
    }
}

// Child class: Car inherits from Vehicle
class Car extends Vehicle {
    constructor(brand, year, doors, fuelType = "gasoline") {
        super(brand, year); // call parent constructor
        this.doors = doors;
        this.fuelType = fuelType;
    }

    // Override getInfo and add car-specific details
    getInfo() {
        // Call parent method first
        const parentInfo = super.getInfo();

        function formatDoors(num) {
            return num === 2 ? "coupe" : `${num}-door`;
        }

        console.log(`Car-specific details:`);
        console.log(`  Type: ${formatDoors(this.doors)}`);
        console.log(`  Fuel: ${this.fuelType.toUpperCase()}`);
        
        return `${parentInfo} - ${formatDoors(this.doors)} ${this.fuelType} car`;
    }

    // New method with nested functions
    drive(distance) {
        function calculateFuelNeeded(distance, efficiency = 30) { // mpg
            return (distance / efficiency).toFixed(2);
        }

        function generateRoute() {
            const routes = ["highway", "city streets", "scenic route"];
            return routes[Math.floor(Math.random() * routes.length)];
        }

        const fuelNeeded = calculateFuelNeeded(distance);
        const route = generateRoute();

        console.log(`Driving the ${this.brand} ${distance} miles...`);
        console.log(`  Route: ${route}`);
        console.log(`  Estimated fuel needed: ${fuelNeeded} gallons (${this.fuelType})`);
    }
}

// Create instances
console.log("=== Creating a basic vehicle ===");
const vehicle = new Vehicle("Generic Motors", 2020);
vehicle.getInfo();
vehicle.startEngine();

console.log("\n=== Creating a car (child class) ===");
const myCar = new Car("Tesla", 2024, 4, "electric");
myCar.getInfo();        // calls overridden method
myCar.startEngine();    // inherited from parent
myCar.drive(150);       // child-specific method

console.log("\n=== Another car ===");
const oldCar = new Car("Toyota", 2010, 4);
oldCar.getInfo();
oldCar.drive(50);