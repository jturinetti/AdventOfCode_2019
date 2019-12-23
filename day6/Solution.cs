using System;
using System.Collections.Generic;
using System.Linq;

namespace Day6Solution
{
    public class Solution
    {
        public Dictionary<string, Node> Data {get;}

        public Solution()
        {
            Data = new Dictionary<string, Node>();
        }

        public void ParseInputLine(string input)
        {
            var inputArray = input.Split(')');
            var nodeBeingOrbitedId = inputArray[0];
            var nodeOrbitingId = inputArray[1];

            var nodeBeingOrbited = new Node(nodeBeingOrbitedId);
            var nodeOrbiting = new Node(nodeOrbitingId);

            if (!Data.ContainsKey(nodeBeingOrbitedId) && !Data.ContainsKey(nodeOrbitingId))
            {
                // neither node exists in dictionary yet
                nodeOrbiting.Parent = nodeBeingOrbited;
                nodeBeingOrbited.Children.Add(nodeOrbiting);
                Data.Add(nodeBeingOrbitedId, nodeBeingOrbited);
                Data.Add(nodeOrbitingId, nodeOrbiting);
            }
            else if (!Data.ContainsKey(nodeBeingOrbitedId))
            {
                // parent node doesn't exist
                // child does, get it
                var childNode = Data[nodeOrbitingId];
                // add existing child to new parent node
                nodeBeingOrbited.Children.Add(childNode);
                // add parent                
                Data.Add(nodeBeingOrbitedId, nodeBeingOrbited);
                // set parent on child
                childNode.Parent = nodeBeingOrbited;
            }
            else if (!Data.ContainsKey(nodeOrbitingId))
            {
                // child node doesn't exist
                // parent exists, get it
                var parentNode = Data[nodeBeingOrbitedId];
                // set existing parent
                nodeOrbiting.Parent = parentNode;
                // add child
                Data.Add(nodeOrbitingId, nodeOrbiting);
                // add child to parent's children collection
                parentNode.Children.Add(nodeOrbiting);
            }
            else
            {
                // both exist!
                nodeBeingOrbited = Data[nodeBeingOrbitedId];
                nodeOrbiting = Data[nodeOrbitingId];
                // set parent->child relationship
                nodeOrbiting.Parent = nodeBeingOrbited;
                // add child
                nodeBeingOrbited.Children.Add(nodeOrbiting);
            }
        }

        public int CalculateOrbits()
        {
            var centerOfMass = Data["COM"];

            return CalculateOrbitsImpl(centerOfMass, 0);
        }

        private int CalculateOrbitsImpl(Node currentNode, int currentDepth)
        {
            currentDepth++;
            if (!currentNode.Children.Any())
            {
                // return ((currentDepth ^ 2) + 1) / 2;
                return currentDepth;
            }

            return currentNode.Children.Sum(n => CalculateOrbitsImpl(n, currentDepth));
        }
    }

    public class Node
    {
        public Node(string id)
        {
            Id = id;
            Children = new List<Node>();
        }

        public string Id {get;}

        public Node Parent {get;set;}

        public List<Node> Children {get;set;}
    }
}