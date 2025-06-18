# Discussion

### What are the limits of operation of the service you developed (ex: concurrent users, memory usage)?

Limited by Erlang’s concurrency model.<br> 
DynamoDB’s provisioned throughput.<br> 
Memory usage depends on the number of active connections and data size.

###  Your service implementation is scalable, i.e., it’s able to handle a large number of  concurrent users on multiple servers? How can/did you achieve scalability, software and infrastructure wise?

Deploy multiple instances behind a load balancer. <br> 
DynamoDB auto-scaling.

### What was more challenging in the development of the service?

Learning AWS. <br> 
Integrating Erlang and ensuring correct message encoding/decoding. <br> 
Learning KMS encryption and CloudFormation for secure deployment. <br>

### If you had infinite time how would you improve the service?

Add connection pooling for DynamoDB to optimize latency. <br> 
Implement rate limiting and monitoring. <br> 
Support batch operations for efficiency. <br> 
Compress data. <br> 
Connection pooling.
