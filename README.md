# HSChain

<b> Simple blockchain server implemented in Haskell</b>  
creates a peer-to-peer network that synchronizes the blockchain.
## Build
```sh
stack build
stack exec hschain httpPort p2pPort [optional seed]
```

## E.g
1. set up two connected nodes.
```sh
hschain 7000 7001

# seed for bootstrapping a connection in a p2p network. 
hschain 8000 8001 localhost:7001
```

2. mine block
```sh
# add a new block to the blockchain via a POST request
curl -H "Content-Type: application/json" -X POST http://localhost:7000/block -d "{\"block\" : \"some data\"}"

# check the blcokchain via a GET request 
curl -X GET http://localhost:7000/chain
```