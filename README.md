# Delay Tolerent Network Project
This is a project for CMPT436 at UofS that is centered around the creation of a delay tolerant network for interplanetary networking applications. 

## Background
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
