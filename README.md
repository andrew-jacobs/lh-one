# LH-One - A Model Lighthouse Controller

This is a silly project my friend Robert Frost and I dreamed up one night in [Reading Makerspace](http://www.rlab.org.uk). Our cunning plan was to sell it as a simple electronics kit (a PCB, a bag of bits and some instructions) on eBay.

The circuit is very simple. A small PIC micro-controller generates three PWM signals to the pins of an RGB LED via some load resistors. A tact switch lets you select the pattern (or ['Light Character'](https://en.wikipedia.org/wiki/Light_characteristic)) to be displayed.

![Schematic](schematics/LH-ONE.png)

I put together a small two-sided PCB to keep things neat and tidy.

![Partially Populated PCB](images/lh-one.png)

The four pin header provides power (5V) and the option of an RS-232 connection which I was going to use to let users program their own light character using a built in 'monitor'. You can connect to the board using a PL2303 or FTDI232RL USB serial module and a terminal emulator like TeraTerm. 

Someone in the hackspace thought it needed a model to show it off and 3D-printed a lighthouse which I painted with a fetching set of stripes.

![Video](video/video.gif)

The software contains a selection of light characters for UK lighthouses based on the information at [Trinity House](https://www.trinityhouse.co.uk/lighthouses-and-lightvessels?type=lighthouse#filters). I live inland and can't easily compare my interpretation of the patterns to the real thing. Please let we know if I'm way off the mark.

Sadly Bob passed away in May 2017 following a stroke and the project was not finished.

In memory of my friend I've decided to clean up the code, schematics and PCB design and release it all on GitHub. 

## Operation

LightHouse | Character
-----------+-----------
Alderney | Fl(4) 15s


## Monitor



