# **Delay Tolerent Network Project**
*This is a project for CMPT436 at UofS that is centered around the creation of a delay tolerant network for interplanetary networking applications.* 

## **Background**
With the ever expanding push further and further away from cozy, well maintained, (relatively) delay free networks of terrestrial earth and near earth orbit, the need for a framework to manage communications increases in parallel. Current systems are not up to snuff against the delay in message received and response times. With a rate of 8.317 light minutes per AU the data transit times for significant stellar bodies are as follows:

| Planet | Max AU Distance | Min AU Distance | Max Transmission Time |  Min Transmission Time | 
|---------|:------------------------:|:----------------------:|:-------------------------------:|:------------------------------------:| 
|Mercury|1.39|0.61|11.56|5.07| 
|Venus|1.72|0.28|14.30|2.33| 
|Mars|2.52|0.52|20.96|4.32| 
|Jupiter|6.20|4.20|51.56|34.93| 
|Saturn|10.54|8.54|87.66|71.02| 
|Uranus|20.22|18.22|168.16|151.53| 
|Neptune|31.06|29.06|258.32|241.68| 

With http timeout times being anywhere from 120 seconds to 5 seconds, there is no chance of a request to go through without timeouts.

[Link to Sun-Planet Distance](https://en.wikipedia.org/wiki/Planet)

[Link to Timeout Times](https://www.ibm.com/support/knowledgecenter/SSPREK_7.0.0/com.ibm.itame.doc_8.0/ameb_webseal_guide/reference/ref_timeouts.html)

## **Goals**

### **Primary:**

##### <img src="https://image.flaticon.com/icons/svg/490/490366.svg" width="64" >**&nbsp; &nbsp;Build an Easily Expandable System** 

* [ ] As this system will be designed in a manner that is not specific to a particular communication protocol, transmission/receiving medium, or other network specific technology, the ability to swap out, add, remove and use multiple different system components is key. For the purpose of this project, the system will have specific ability to facilitate the expansion of the following aspects:
  * [ ] Encoding Protocol: As a varying array of encoding schemes may be needed for different applications, the system should have the capability to add, remove and change schemes when necessary. 
  
  * [ ] Transmission/Receiving Capabilities: As it would be hard to determine  universal physical transmission capabilities for the already 2000+ satellites in orbit now, with undefined numbers in the future, the system should be extremely versatile in its capabilities to accept different custom coded transmigration interfaces.
  

##### <img src="https://image.flaticon.com/icons/svg/129/129639.svg" width="64" >**&nbsp; &nbsp;Provide a Reliable Uplink, Downlink and Interlink Systems** 

* [ ] Uplink: Communication from ground systems to orbital or other distant systems is the first of the three pivotal communication hurdles for the system. The uplink system will be traffic from the more dependable terrestrial networks first point of contact for outgoing data communications.

* [ ] Downlink: Communication to ground systems from the near or deep space networks is the second pivotal communication hurdles for the system. All date with a dependable, non-latent network final destination will pass through the downlink system.

* [ ] Interlink: Communication between domain groups in the deep space network is the third pivotal communication hurdle for the system. With a large majority of network traffic being between domain groups, this will be the work horse of the system. Subsystems of this would include: 
  * [ ] Latency Management: Mechanisms to deal with the previously listed latency rates between celestial bodies or other satellites 
  
  * [ ] Packet Reassembly: Packets are bound to get lost or corrupted in the depths of space, thus the system must be able to reassemble and error check any received data to maintain reliability and message quality.
  
  * [ ] Packet Relay: Once a message is received, if the receiver is not the final destination, the system will have to relay it to an appropriate domain group to help get it on its way. 

##### <img src="https://image.flaticon.com/icons/svg/149/149206.svg" width="64" >**&nbsp; &nbsp; Storage Systems and Memory Management**

* [ ] Storage System: As reliability of the network will be questionable at best, storage of messages will need to take place for possible re-transmission.
 
* [ ] Systems and Memory : As memory will not be infinite, there should be some basic system memory management.

### **Supplementary:**

##### <img src="https://image.flaticon.com/icons/svg/131/131079.svg" width="64" >**&nbsp; &nbsp; Power Usage Optimization**

* [ ] Power Usage Optimization: As hardware the system will be running on will have extreme power constraints, some consideration into maximizing successful transmissions and data management could be beneficial.
 
##### <img src="https://image.flaticon.com/icons/svg/76/76318.svg" width="64" >**&nbsp; &nbsp; Custom Protocol**

* [ ] Create Protocol: As some users will not want to derive their own encoding for messages, a basic protocol optimized for data transmission quality may be a good addition.


**[SVG Authors:](https://www.flaticon.com)** Freepik, Iconnice, Smashicons, and Becris


**Project Authors:** Fredrick Johansson, Nicholas Brunoro, Wai Chung Fung, and Sam Horovatin
