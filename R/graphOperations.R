
function () 
# Most probably this function could be coded in a considerably simpler way
# Given binary network matrix, compute how many separate components it contains.
# Didn't find a ready algorithm for this easily.

# input: binary matrix 'mynet'

# Outline:
# start from first gene, list all neighborgs and their neighborgs etc. until no new nodes are added.
# store the original indices of these nodes
# remove component from the network
# repeat the procedure for the parsed network. Also keep the original indices as names for the nodes.

# Give IDs to nodes (= original indices)
nodeIDs=1:dim(mynet)[[1]];

componentNodes = list();
componentSizes = c();
componentCount = 0;
while (length(dim(mynet)[[1]])>0) {

  neighs = which(as.logical(mynet[1,]))
  totneighs = unique(c(1,neighs));
  addedNodes = 1;
  ind=1;
  oldN = length(totneighs); # number of linked genes (neighs)

  while (addedNodes==1) { # continue until no new nodes are added

    # list neighborghs of this node
    neighs = which(as.logical(mynet[totneighs[ind],]))
    # add to the total list of neighborghs
    totneighs = unique(c(totneighs,neighs))
  
    # check next node in the list otherwise continue to beginning of the list 
    if (ind < length(totneighs)) {
      ind = ind + 1
    } else {
      ind = 1;
      if (length(totneighs)>oldN) {
        oldN = length(totneighs);
      } else {
        #no new nodes have been added when going through the list. 
        # full component obtained -> stop.
        addedNodes = 0; 
      }
     }
  }


  # store the names of the component
  componentCount = componentCount + 1;
  componentNodes[[componentCount]]=nodeIDs[totneighs];
  componentSizes[[componentCount]]=length(totneighs);
  # mark identified component nodes
  noncomponentInds = rep(1,dim(mynet)[[1]])
  noncomponentInds[totneighs]=0; # put zero for the component nodes
  noncomponentInds = which(as.logical(noncomponentInds))
  # remove the component from the network
  mynet = mynet[noncomponentInds,noncomponentInds];
  # also store node IDs for the remaining network
  nodeIDs = nodeIDs[noncomponentInds]; 

  # print network size
  dim(mynet)[[1]]

}

#hist(componentSizes,20)
#sum(componentSizes)
#length(componentSizes)
