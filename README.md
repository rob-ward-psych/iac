# IAC
IAC stands for Interactive Activation and Competition, the approach developed by
McClelland and Rumelhart in their model of word reading.



# Basic network structures 
The organisation, connections, and weights of the network, as well as network activation parameters, are specified in a network file. The network file uses a standard format called "YAML" (yet another markup language), described a farther down below. The network can be read in a single call in the R script, like

         net = read_net('jets_sharks.yaml')

The examples in this section refer to the file jets_sharks.yaml, which replicates McClelland's (1981) "jets and sharks" model (https://web.stanford.edu/group/pdplab/pdphandbook/handbookch3.html). 

_Hint:_ When creating networks, you'll often want to check whether the units are being named correctly, and whether the connections being made to the correct pools, and so on. You can use the verbose flag to watch the network creation being executed in the R console. Call it like this:

         net = read_net('jets_sharks.yaml', verbose = TRUE)

It's worth noting that as far as network creation goes, anything you can do in a network .yaml file, you can do in your .R script, and vice versa. But the network file is for me easier to read and digest.

## Pools
A network is organised into pools of units. Pools need to have a *name* and a *shape*. The units inside the pool also need to have names (these are called *unitnames*). 

A first example. Here is a yaml code for two pools of units. Each pool is specified by a dictionary of {key: value} pairs. The *keys* here are name, shape, and unitnames. 

          pools: [
            {name: gang, shape: 2, unitnames: [jets, sharks]},
            {name: job, shape: 3, unitnames: [pusher, burglar, bookie]}
          ]

* _name_: The pool will be referred to by this name in the code, and so this must be a legal variable in R. Therefore no spaces, must not start with a digit, etc.
* _shape_: In the basic case, the shape indicates the number of units in the pool, and is specified by a single integer value (later we see how shape can specify multi-dimensional spatial pools). In the examples above, gang has two units, and job has three units.
* _unitnames_: Every unit in the network needs its own unique (and ideally, sensible) name. Why? Names are used to set network connections, plot activations, and access the network's weight matrix. Although units can be referred to by number, numbers will change with small tweaks to the network, and are easy to get wrong. Referring to units by names is more reliable and robust. Like pool names, unit names need to be legal variable names in R. 

There are several ways to specify unitnames. They can simply be explicitly given as a list, as in the example above. Note that the list of unitnames, like any list, is enclosed in square brackets (not curly, that's a dictionary and used for {key: value} pairs!).

When you just have 2 or 3 units in a pool, it's easy enough to explicitly give all the unitnames. However, as the pools get bigger, or the structures more complicated, this gets awkward. So, beside *unitnames*, there are two other parameters that can help create unitnames in an easy and systematic way: *basenames*, and *prefix*.

* _basenames_ and _prefix_: Use these two parameters instead of specifying unitnames directly. The basic idea is that the unitnames for a pool are created by the combination of prefix+basenames. The default _prefix_ is the pool name, and the default _basenames_ enumerate through the shape of the pool. That's complicated so here's an example:

         {name: gang, shape: 2}
		 
No unitnames have been specified for the gang pool. However, the default *prefix* is the pool name ("gang"), and the default *basenames* is just a sequence of numbers, from 1 to the number of units in the pool, in this case 2. We combine the prefix with the basenames, and the resulting unitnames are gang1 and gang2. This isn't nearly as good as the unitnames "jets" and "sharks" but this also took 0 effort to specify.

Here's a more realistic example. In the original jets and sharks network there is a pool of gangster _names_ (e.g., Al, Fred). There is a separate pool of gangster _instances_, specified with a leading underscore, e.g., \_Al and \_Fred. The instance units are connected to every relevant trait of a particular person. If Al is a member of the jets, then the Al instance unit (\_Al) is connected to the "jets" unit in the _gang_ pool, and to the "Al" unit in the _names_ pool. 

All that as preamble, here's how we might specify the _instances_ pools:

         {name: instance, shape: 4, prefix: _, 
	         basenames: [Art, Al, Sam, Clyde]}

Now the prefix \_ is placed in front of the basenames, producing unitnames _Art, _Al, _Sam, and _Clyde.

However you specify the unitnames, remember they need to be legal R names, so that you can plot and work with the unit activations. So don't use a prefix starting with a digit, no spaces, etc.

## Connections
Connections specify the weights between units. The syntax is similar in many ways to pools. There is a list called "connections" and each item in the list is a dictionary that describes the connection through {key: value} pairs.
Here's a snippet from jets\_sharks.yaml

           connections: [
             {from: _Art, to: [Art, jets, in40s, jh, single, pusher], weight: 1.0},
             {from: _Al, to: [Al, jets, in40s, jh, single, burglar], weight: 1.0},
           ]

A connection must specify three things: the "from" units, the "to" units, and the "weight" between them. The _from_ and _to_ can be a single pool or unit, or a list of units. In jets\_sharks, connections are generally between the "instance" units, representing a particular person, and each of the units associated with that person's traits. The first connection in the example above connects the instance unit "_Art" to a list containing all of Art's associated units in other pools: name, age, marital, and job. 
				  
### Directives
There are also a few optional "directives" which can modify  connections. The legal directives are: oneway, reciprocal, self, others. 
            
##### _oneway_ and _reciprocal_
By default, connections are reciprocal. A connection from _Art to pusher, also connects pusher to _Art. If you want to specify the connections in just one direction, then add the directive "oneway" as shown below. 

         # not used in jets_sharks
         [from: _Art, to: [Art, jets, in40s, jh, single, pusher], 
              weight: 1.5, directives: oneway}
You can explicitly direct a connection to be reciprocal, but it isn't necessary.

##### _self_ and _others_
A unit or pool can certainly be connected to itself. For example, the line below would connect every unit in the "instance" pool to every unit in the "instance" pool, including itself, with an inhibitory weight.

       {from: instance, to: instance, weight: -1.0}

The use of the "self" and "other" directives gives a  little more control when making within-pool connections. The "self" directive means the weight will only be a self-connection, from each unit to itself. And the "others" directive means the weight will be applied from the unit to all other units in the pool, *except* itself:

        {from: instance to: instance, weight: -1.0, directives: others}
        {from: instance, to: instance, weight: 0.1, directives: self}  # not used in jets_sharks
        
The above examples means every unit in the "instance" pool is connected with a -1 weight to every other unit in that pool. However, the self-connection is excluded by the use of the "others" directive. The next line, by using the "self" directive, gives each unit a self-excitatory connection of 0.1. This is the kind of contrast-enhancement that would promote a winner-takes-all outcome within this pool. 

When using self and others within a pool, the reciprocal and oneway directives don't make sense and are ignored.

Finally there is a directive called "add", which adds the _weight_ to the specified connection rather than simply specifying it. This is handy in some niche situations, so I'll come back to this one later.

____

## Parameters
There are several essential parameters that must be specified for the network to run. These parameters have the same name and meaning as those used by McClelland (1981). They can be specified in the R code, and often easier, they can be assigned within a network .yaml file, in in a parameters section. This is a dictionary, a set of key: value pairs enclosed in brackets.

          parameters: {
            max:   1.0,
            min:  -0.2,
            rest: -0.1,
            decay: 0.1,
            estr:  0.4,
            alpha: 0.1,
            gamma: 0.1,
            noise: 0 
           }
            
_max_ and _min_ give the maximum and minimum activation values. _rest_ is the baseline activation level, and unit activity will gradually move towards this baseline based on the strength of the _decay_ parameter. _alpha_, _gamma_, and _estr_ weight different kinds of input: _alpha_ weights input from positive weights, _gamma_ from negative weights, and _estr_ from external input sources. Finally, _noise_ can add random gaussian noise to a unit input.

It can be very convenient to manipulate parameters within an R script. Two forms for parameter setting are shown below, they work equally well. Just substitute noise or decay for the parameter you're interested in.

            net = read_net('jets_sharks.yaml')
            net$params$noise = 0.1
            net$params[['decay']] = .25

# Referring to networks and their properties within R code
## cycle(), read_net(), and other functions
The R functions that work with the iac network almost always take a first argument which is the network being worked with, and return a modified version of the network. For example:

             net = cycle(net, ncycles = 100)
			 
Why assign the results of cycle(net) back to net? This might seem odd and circuitous, but it is necessary. The reason is that R does not modify variables passed as arguments (except under very special conditions). To change a variable, as a rule, you need to explicitly assign it a new value, not simply pass it as an argument to a function. So if we had mistakenly said:

             net = read_net("jets_sharks.yaml")
             cycle(net, ncycles = 100) # mistake
             plot_activations(net)
This code runs fine, but the output of the cycle() function in the second line isn't assigned anywhere. After cycle() is complete, the variable net is just the same as it was in the first line, when read_net() was called. When plot\_activations(net) is called, the plot therefore shows no activity and no cycles.

## Weights
Within the R code, individual weights can be examined and set in the 2D matrix _weights_. Rows in this matrix represent the sender (or from) unit, columns the receiver (or to) unit. Entries in the matrix are best referred to by their unitnames.
For example

            net = read_net('jetss_harks.yaml')
            net$weights['Ken', 'burglar'] = 5
            net$weights['burglar', c('_Ken' , '_Al')] = 0
This peculiar example would set the weight from Ken to burglar to 5; and the weight from burglar to _Ken and _Al to 0

# Networks with spatial organisation
The jets and sharks model has no spatial structure, just bags of units. But you can create networks with spatial maps. Pools can be flexibly organised into spatial arrays of units, to allow system connections within and between pools. Specifying spatial structure and connections is described in a separate doc, iac\_spatial_networks.md

# YAML basics
YAML is designed for structured information in human and computer readable form.
## Lists and dictionaries, lists of dictionaries, and lists inside dictionaries
Networks can be created in R code, but it is probably easier to create them using a .yaml network specification file. YAML (yet another markup language) is designed to encode structured information in a way that is relatively easy for both humans and computers to read. Rather than worrying too much about YAML, it is probably easier to work with examples (jets\_sharks.yaml and others). But a few basics are covered here.

There are two main types of structures in YAML, lists and dictionaries. Lists are indicated by a square brackets [] and consist of a comma-separated series of items. The other type of structure is called a "dictionary", or "associative array". A dictionary consists of "key: value" pairs, separated by commas, and enclosed in curly brackets {}. YAML is pretty flexible and there are other ways to specify lists and dictionaries, but for now we'll stick with these.

Very important is that lists and dictionaries can be freely embedded inside one another: a dictionary can contain lists and those lists could contain more dictionaries, and those dictionaries could contain other dictionaries, which contain other lists, as far as you care to go. In fact, networks are specified using lists of dictionaries.

          pools: [
            {name: gang, shape: 2, unitnames: [jets, sharks]},
            {name: job, shape: 3, unitnames: [pusher, burglar, bookie]}
          ]

The example above shows a list called "pools". The list is indicated by the [] brackets. The "pools" list consists of a sequence of dictionaries. Each dictionary is enclosed in {}, and separated by commas. A dictionary consists of a set of what are called key: value pairs. For example, "name: gang", is a key: value pair. You can see there are two units in the gang pool, one for jets and one for sharks. Note also that there is a space between the key and the value, it's _name:_ **space** _gang_, not _name:gang_.

### Aliases
A handy feature of YAML is that it lets you make aliases when you have repeating text (sometimes referred to as anchors and aliases). This feature can make the network file shorter, easier to specify, and reduce input errors. There is an example in jets_sharks.yaml, where the gangster names are specified once and then used for both the "name" and the "instance" units.  

Aliases are nice for other purposes too. For example, you might want to allow different weights for self-inhibition in different pools. The weight for each pool can be assigned to an alias, making then easy to change. Setting and using aliases is shown below (although these values were not used in the original model):

      alias: [
	  &within_instance  -1
	  &within_job -1.5
     ]
     connections: [
	 {from: instance, to: instance, weight: *within_jobs, directives: others},
	 {from: job, to: job, weight: *within_jobs, directives: others},
	 ]


## R code or .yaml network files?
Anything that can be done in a .yaml file can be done using R commands within a script. This is probably less convenient for initially creating networks, but can be important if you want to manipulate the network while running an experiment. The "spatial examples" folder has an example. The file what\_where\_coder.R shows how the exact same network created by read\_net('what_where.yaml') can be created purely within an R script. 

------------

# Networks with spatial organisation
Networks with spatial organisation can be specified. Pools can be flexibly organised into spatial arrays of units, to allow systematic connections within and between pools. 

For example, for pools with corresponding spatial structures, we could have excitatory connections limited to units representing the same spatial location. Within a spatially-organised pool, we might have nearby units inhibit each other more strongly than distant ones. 

## Example: the what\_where network
To show how the options for spatial organisation work, we'll be mainly sticking to the example network file "what_where.yaml". This network has a set of spatially-organised "feature maps", which are connected to a "what" stream of global feature identities, and to a "where" stream consisting of a spatially-organised "salience map".

This example imagines a world and network as follows:

1. The visual environment is a (very) low-res "world" consisting of a spatial grid of 2x3 possible locations.
2. To process this world, the network has a "salience map" pool. This map will register WHERE information -- whether there is any activity in these 2x3 world locations. The salience pool is therefore a two-dimensional, 2x3 pool of units, each unit representing one location. 
3. External input from the world is registered on the network's "feature maps". The feature maps are similar to a retina for the network. These maps represent the occurrence of a specific feature at a specific location. In this example, there are two feature maps, one for red features, and one for blue. Each feature map also has a 2x3 structure, but unlike the salience pool, the units in a feature map are activated only by their specific visual feature at a specific location, i.e., WHAT x WHERE. It's important to see that the feature map pool is a *three*-dimensional structure: the number of features being represented (two: red and blue) x the 2x3 locations of the 2D visual world.

An overview of the **what_where** network with some sample input: a red blob at location 1,1 and a blue blob at location 2,2. In these diagrams, O=an activated unit; •=an inactive unit.

       Feature maps  
       WHAT x WHERE
       red map    blue map   
       --------    -------    The feature maps show
      | O • • |   | • • • |   something red is at [1,1] 
      | • • • |   | • O • |   and blue at [2,2]
       --------    -------
	   
       WHAT                   WHERE
        ---------              -------
       | red   O |            | O • • |  
       | blue  O |            | • O • |    
        ---------              -------
       Feature ID (2 units)   Salience map                
       - Both red and blue    - Objects are present 
         identities are         at both location [1,1]
         active                 and location [2,2]   

Units in the different pools represent different forms of information:

- **Feature maps**: the presence of a specific feature at a specific location (or **WHAT x WHERE**)
   - A blue blob in the lower centre = location [2,2] in the blue map
- **Feature ID**: the presence of a feature, but not its location (**WHAT**)
   - The blue ID unit is activated by (and itself activates) all units in the blue feature map
- **Salience**: indicate the location but not the identity of features (**WHERE**)
   - Activity in location [2,2] of the salience map could be due to activity at [2,2] in either the red or blue feature maps (or even both)

Another way to describe it.. units in the WHAT pool (feature ID)represent the presence of a feature, but not its location. The grid of units in the WHERE pool (salience map) represent activated locations, but give no information about feature ID. A unit in the feature maps specifies BOTH the location and identity of a feature.


# How this network is specified
We now work through the entire specification in what\_where.yaml

## Pools: Shape and names
In pools without spatial structure, the _shape_ parameter simply gives the total number of units in the pool. But in a spatially-organised pool, _shape_ gives the dimensions of a multi-dimensional spatial array of units. From what\_where.yaml:

     pools: [
      {name: feature_ID, shape: 2, unitnames: [red, blue]},
      {name: salience_map, shape: [2,3], prefix: s_},
      {name: feature_maps, shape: [2,2,3], prefix: fm_,
        basenames: [[red_, blue_], [1,2], [1,2,3]]}
     ]
                
There's nothing new here with the feature\_ID pool. But quite a lot new in the others! In salience and feature\_maps, the *shape* is a list giving the size of each dimension in the pool. The pool salience is a 2D array, 2x3 units in size = 6 total units. The pool feature\_maps is a 3D array. For each of two maps (red and blue) there are separate units representing the 2x3 array. This pool therefore has 2x2x3 = 12 units.

The next thing to note is how prefix and basenames are being used in the spatial pools. The prefix is used just as before, but with a multi-dimensional pool, basenames needs to reflect *each dimension*. Look first at the feature\_maps: basenames is list of lists, indicating how each dimension should be named. The shape of this pool is three-dimensional: 2 (maps) x 2 (rows) x 3 (cols). The first list in basenames gives the names for each map (red\_, blue\_). The second list and third list are just sequences of numbers for the rows and columns of the map. The name for unit in the 3D array is the prefix + the corresponding characters in the basename lists. So for unit [1, 2, 1] in the feature maps, the unitname will be 'fm\_' (the prefix) + 'red\_' + '2' + '1' = fm\_red\_21. 

We could just as well have specified the basenames as something really wordy like:

        basenames: [[red_, blue_], [upper,_ lower_], [left, middle, right]] }

Unit [1, 2, 1] in the feature map pool would then have the name: fm\_ + red\_ + lower\_ + left_ = 

         fm_red_lower_left

You can always inspect the names being given to your units in the R console:

     nn = read_net('what_where.yaml')
    nn$unitnames

or if you are just interested in a single pool, like feature_maps:

    nn$pools$feature_maps$unitnames

NOTE: This example uses pretty verbose unitnames for the feature maps. This is to try to make it clearer how multi-dimensional basenames relate to the multi-dimensional pools. However in practice I would use shorter names:

        {name: feature_maps, shape: [2,2,3], prefix: f, basenames: [[r, b], [1,2], [1,2,3]]}
		
Producing much nicer and easier-to-type unitnames like:

      fr11, fr12, fr13, ... fb11,  fb12, fb13...
        
## Spatial connections 

The whole point of using spatial pools is to allow systematic connections, so that connections between units representing the same (or nearby) spatial coordinates can be easily specified.

Let's go back to the salience and feature\_maps example. The pool salience is a 2x3 array, and feature\_maps is a 2x2x3 array. We could set up something analogous in R, something like:

       salience = array(data = 0, dim = c(2, 3))
       feature_maps = array(data = 0, dim = c(2, 2, 3))

Suppose we wanted to use this R code to represent a visual world with a red feature at location 1,1 and a blue feature at location 2,3. The salience map should reflect the presence of both features.

       red = 1; blue = 2;
       feature_maps[red, 1, 1] = 1.0
       feature_maps[blue, 2, 3] = 1.0
       salience[1, 1] = 1.0
       salience[2, 3] = 1.0

The key point is that we are representing corresponding spatial locations using the second and third dimension of feature\_maps and the first and second dimensions of salience. This idea of corresponding dimensions is key for setting up spatially-organised connections. 

Let's look at the connections between the feature and salience maps in what\_where.yaml:

     connections: [
     {from: feature_maps, to: salience, weight: 1.0, from_dims: [2,3], to_dims: [1,2]}
     ]

What is new here is the use of **from\_dims** and **to\_dims**. These parameters specify how the different dimensions of the "from" and "to" pools match up. This line is saying that the second and third dimensions of the feature maps pool (the 2x3, width x height) corresponds to the first and second dimension of the salience pool (again the 2x3 width x height). Just like in the R code.

This instruction connects the units in the salience map to the corresponding units in the red and blue sections of the feature maps, as indicated by from\_dims and to\_dims. It achieves **exactly** the same thing as painstakingly specifying the corresponding connections one at a time, like this:

    connections: [
     {from: s11, to: [fm_red_11, fm_blue_11], weight: 1.0},
     {from: s12, to: [fm_red_12, fm_blue_12], weight: 1.0},
     {from: s21, to: [fm_red_21, fm_blue_21], weight: 1.0},
     {from: s22, to: [fm_red_22, fm_blue_22], weight: 1.0},
     {from: s31, to: [fm_red_31, fm_blue_31], weight: 1.0},
     {from: s32, to: [fm_red_32, fm_blue_32], weight: 1.0}
     ]

Doing things one connection at a time will work, but is error-prone, and also a real pain to revise, for example if you later decide you want a 5x5 visual world. In contrast, the connections line above, using from\_dims and to\_dims wouldn't need to be changed at all!

### Spatially-varying weights
The other handy thing about specifying spatial connections using from\_dims and to\_dims is that you can have the weight of connection vary with the distance between corresponding unit locations. For example, you could implement a kind of contrast-enhancement, so that units in the salience maps positively activated their corresponding location in the 
feature maps, but inhibited nearby locations. That could easily be done as follows:

     connections: [
     {from: salience, to: salience, weight: [1.0, -.5, -.1],  
		 from_dims: [2,3], to_dims: [1,2]}
     ]

We've now specified weight as a list: [1.0, -.5, -.1]. This means that units in salience and feature\_maps corresponding to identical locations are connected by a weight of 1.0; units representing locations that are 1 distant from each other in those maps are connected by a weight of -.5; units two away from each other by a weight of -0.1. 

This achieves the same thing as the **unbearably** **horrible**:

    connections: [
     {from: s11, to: [fm_red_11, fm_blue_11], weight: 1.0},
     {from: s11, to: [fm_red_12, fm_blue_12, r21, b21], weight: -0.5},
     {from: s11, to: [fm_red_22, b22, r31, b31], weight: -0.1},
     {from: s21, to: [fm_red_21, b21], weight: 1.0},
     {from: s21, to: [fm_red_22, b22, r11, b11, r31, b31], weight: -0.5},
     {from: s21, to: [fm_red_32, b32], weight: -0.1},
      etc..... 12 more lines of this stuff!

That's a real **mess**, very easy to screw up, and very hard to revise, e.g., if you wanted to increase the size of the spatial maps, or change how weights vary with distance. Whereas using from\_dims and to\_dims, it's just one line that can be easily modified if needed (and for many changes it won't be, for example if you changed the size of the maps from 2x3 to some other value).

Distance is calculated using a city-block metric, e.g, location [1,1] and [3,3] would have a distance of 4. 

### The "add" directive
Finally, occasionally it's been useful to add (or subtract) from the current value of a connection rather than set its value. In one application with feature maps, I wanted inhibition between red and blue parts of the feature map to inhibit each other weakly, but red to inhibit red and blue to inhibit blue more strongly. There are different ways to do this, but the easiest is probably the "add" directive. "Add" works out the units being connected as normal, but adds the weight value rather than replacing it.

    {from: feature_maps, to: feature_maps, from_dims: 1, to_dims: 1, weight: -.5, 
		directives: [add, others, oneway]},

This specifies the connections of interest as units that share the same index for the first dimension of feature\_maps (the dimension specifying red or blue). Those units are then adding -.5 to their connection weight.



