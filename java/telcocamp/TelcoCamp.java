package telcocamp;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static telcocamp.TelcoCamp.Fruit.*;

/*
 * enum Fruit {APPLE, BANANA, CUCUMBER}
 * 
 * class Animal {
 *     listFruit(); // prints all fruit
 *     give(Fruit); // gives new fruit to animal
 *     eat();       // eats last fruit from inventory
 *     eat(Fruit);  // eats the provided fruit from inventory
 * }
 * 
 * class Horse extends Animal;  // eats apples only
 * class Monkey extends Animal; // eats bananas only
 * class Human extends Animal;  // eats all fruit
 * 
 * class Hoarder extends Human;  // eats nothing
 * class Devourer extends Human; // instantly eats everything
 * class Sick extends Human;     // dies upon eating anything
 *
 */

public class TelcoCamp {

    enum Fruit {
        APPLE, BANANA, CUCUMBER
    }

    class Animal {
        List<Fruit> inventory = new ArrayList<>();
        List<Fruit> edible;

        Animal(Fruit[] edible) {
            System.out.println();
            this.edible = Arrays.asList(edible);
        }

        void listFruit() {
            say("I have: " + inventory.toString());
        }

        void give(Fruit fruit) {
            say("I got a " + fruit + ". Yay!");
            inventory.add(0, fruit);
        }

        void eat() {
            if (inventory.isEmpty()) {
                say("I have no fruit to eat.");
            } else {
                Fruit fruit = inventory.get(0);
                if (edible.contains(fruit)) {
                    say("I ate a " + fruit + ". Yummy!");
                    inventory.remove(fruit);
                } else {
                    say("I do not want to eat " + fruit + ".");
                }
            }
        }

        void eat(Fruit fruit) {
            if (inventory.isEmpty()) {
                say("I have no fruit to eat.");
            } else if (!inventory.contains(fruit)) {
                say("I do not have a " + fruit + ".");
            } else if (!edible.contains(fruit)) {
                say("I do not want to eat " + fruit + ".");
            } else {
                say("I ate a " + fruit + ". Yummy!");
                inventory.remove(fruit);
            }
        }

        @Override
        public String toString() {
            return this.getClass().getSimpleName();
        }

        void say(String string) {
            System.out.println(this + ": " + string);
        }
    }

    class Horse extends Animal {
        Horse() {
            super(new Fruit[]{APPLE});
        }
    }

    class Monkey extends Animal {
        Monkey() {
            super(new Fruit[]{BANANA});
        }
    }

    class Human extends Animal {
        Human() {
            super(new Fruit[]{APPLE, BANANA, CUCUMBER});
        }
    }

    class Hoarder extends Human {
        @Override
        void eat() {
            complain();
        }

        @Override
        void eat(Fruit fruit) {
            complain();
        }

        private void complain() {
            say("No! Leave my precious fruit alone!");
        }
    }

    class Devourer extends Human {
        @Override
        void give(Fruit fruit) {
            super.give(fruit);
            eat(fruit);
        }
    }

    class Sick extends Human {
        @Override
        void eat() {
            int length = inventory.size();
            super.eat();
            if (length != inventory.size()) {
                say("I should not have eaten this...");
                throw new RuntimeException("HORRIBLE DEATH");
            }
        }

        @Override
        void eat(Fruit fruit) {
            int length = inventory.size();
            super.eat(fruit);
            if (length != inventory.size()) {
                say("I should not have eaten this...");
                throw new RuntimeException("HORRIBLE DEATH");
            }
        }
    }

    static void testHorse() {
        Horse horse = telcocamp.new Horse();
        horse.eat();
        horse.give(APPLE);
        horse.eat();
        horse.give(APPLE);
        horse.eat(APPLE);
        horse.give(APPLE);
        horse.give(BANANA);
        horse.eat();
        horse.eat(APPLE);
    }

    static void testMonkey() {
        Monkey monkey = telcocamp.new Monkey();
        monkey.eat();
        monkey.give(BANANA);
        monkey.eat();
        monkey.give(APPLE);
        monkey.eat(APPLE);
        monkey.give(APPLE);
        monkey.give(BANANA);
        monkey.listFruit();
        monkey.eat();
        monkey.eat(BANANA);
    }

    static void testHuman() {
        Human human = telcocamp.new Human();
        human.give(APPLE);
        human.give(BANANA);
        human.give(CUCUMBER);
        human.eat(BANANA);
        human.eat(BANANA);
        human.listFruit();
        human.eat();
        human.eat();
        human.eat();
    }

    static void testDevourer() {
        Devourer devourer = telcocamp.new Devourer();
        devourer.give(APPLE);
        devourer.give(BANANA);
        devourer.give(CUCUMBER);
        devourer.eat();
        devourer.listFruit();
    }

    static void testHoarder() {
        Hoarder hoarder = telcocamp.new Hoarder();
        hoarder.give(APPLE);
        hoarder.give(BANANA);
        hoarder.give(CUCUMBER);
        hoarder.listFruit();
        hoarder.eat();
        hoarder.eat(CUCUMBER);
        hoarder.listFruit();
    }

    static void testSick() {
        Sick sick = telcocamp.new Sick();
        sick.give(APPLE);
        sick.give(CUCUMBER);
        sick.listFruit();
        sick.eat(BANANA);
        try {
            sick.eat();
        } catch (RuntimeException e) {
            sick.say("MEDIIIIIC!!!1");
        }
    }

    private static TelcoCamp telcocamp = new TelcoCamp();

    public static void main(String[] args) {
        testHorse();
//        testMonkey();
//        testHuman();
//        testDevourer();
//        testHoarder();
//        testSick();
    }

}
