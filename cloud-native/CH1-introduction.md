# Introduction to Cloud Native

## Fallacies of cloud computing
pg. 2, 3

A couple of the interesting fallacies of cloud computing:
- latency is zero
- the network is secure
- the topology does not change
    - machines are not known (e.g. static IPs)
    - pets vs cattle
- there is one administrator
- transport cost is zero
- the network is homogenouspg. 4

## Twelve factor app
pg. 4-6

The twelve factor app is effectively a modern, cloud application. By dividing our application across a network partition and isolating small reproducible units of the application we gain desirable properties such as availability, robustness, and scalability. However, to operate effectively in this environment we need to make sure that our services are: reproducible, decoupled, and building is automated.

Outline of the twelve factor app:
- One codebase tracked in revision control; many deploys
- Explicitly declare and isolate dependencies
- Store configuration in the environment
- Treat backing services as attached resources
    - Services that are consumed over a network should be accessed via external configuration
- Separate build and run stages (these should be fully automated)
- Execute the app in one or more stateless processes
- Each service manages its own data
- Concurrency; scale out not up
- Maximize robustness with fast startup and graceful shutdown
- Dev, staging, and prod should be as similar as possible
- Treat logs as event streams
- Run admin and management tasks as one-off processes
