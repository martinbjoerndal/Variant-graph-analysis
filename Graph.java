/**
 * ANALYSES FOR MASTER'S THESIS
 * ––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
 *
 * Graph.java
 * @author Martin Gjesdal Bjørndal
 * 
 * The actual graph built by BuildGraph.java/EditDistance.java/Gedevograph.java
 *  
 **/

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Arrays;

import java.io.PrintWriter;
import java.io.IOException;


public class Graph {
    HashMap<String, Node> nodeList;

    private int NUMBER_OF_NODES;
    private int NUMBER_OF_EDGES;   

    Graph(HashMap<String, Node> nodeList) {
	this.nodeList = nodeList;
    }
    
    public void printNodes() {
	Node[] sorted = new Node[nodeList.size()];
	int[] nodePositions  = new int[nodeList.size()];

	int counter = 0;
	for (Node n : nodeList.values()) {
	    nodePositions[counter] = n.getPos();
	    counter++;
	}

        Arrays.sort(nodePositions);
	for (int i = 0; i < nodePositions.length; i++) {
	    if (i > 0) { 
		if (nodePositions[i] == nodePositions[i + 1]) {
		    Node n = findNodeAtPosition(nodePositions[i]);
		    
		    if (n.getType().equals("alt")) {
			Node alt = n;

			// Remove to get access to the next at the same 
			// position. Then add again
			nodeList.remove(alt.getId());
			Node ref = findNodeAtPosition(nodePositions[i]);
			nodeList.put(alt.getId(), alt);

		    } else if (n.getType().equals("ref")) {
			Node ref = n;
			nodeList.remove(ref.getId());
			findNodeAtPosition(nodePositions[i]).display();
			nodeList.put(ref.getId(), ref);

		    } else {
			findNodeAtPosition(nodePositions[i]).display();
		    }

		    i++;

		} else {
		    findNodeAtPosition(nodePositions[i]).display();
		}
	    } else {
		findNodeAtPosition(nodePositions[i]).display();
	    }
	}
    }

    public Node findNodeAtPosition(int pos) {
	for (Node n : nodeList.values()) {
	    if (n.getPos() == pos) {
		return n;
	    }
	}

	return null;
    }

    public void setNodeNumbers() {
	int nodeNumber = 1;

	for (Node n : nodeList.values()) {
	    n.setNumber(nodeNumber);
	    nodeNumber++;
	}
    }

    /**
     * This is a bit important, as it will be used as input to igraph in R.
     * Then the igraph object can be visualized by R.
     *
     **/
    public void igraphOutput(String graphName, boolean gedevo) {

	if (!gedevo) {
	    try {
		PrintWriter edgeWriter = new PrintWriter(graphName + 
							 "edgeList.txt", "UTF-8");
		PrintWriter labelWriter = new PrintWriter(graphName + 
							  "labelList.txt", "UTF-8");
		edgeWriter.println("fromNode toNode");
		labelWriter.println("seq");

		for (Node n : nodeList.values()) {
		    Edge[] edges = n.getEdges();

		    if (edges != null) {
			for (int i = 0; i < edges.length; i++) {
			    edgeWriter.println(Integer.
					       toString(edges[i].getFromNode().
							getNodeNumber())
					       + " " + Integer.
					       toString(edges[i].getToNode().
							getNodeNumber()));
			}
		    }

		    labelWriter.println(n.getSeq());
		}
	    
		edgeWriter.close();
		labelWriter.close();

	    } catch (IOException e) {
		e.printStackTrace();
	    }

	} else {

	    try {
		PrintWriter edgeWriter = new PrintWriter(graphName + 
							 "edgeList.txt", "UTF-8");
		PrintWriter labelWriter = new PrintWriter(graphName + 
							  "labelList.txt", "UTF-8");
		edgeWriter.println("fromNode toNode");
		labelWriter.println("seq");

		for (Node n : nodeList.values()) {
		    Edge[] edges = n.getEdges();

		    if (edges != null) {
			for (int i = 0; i < edges.length; i++) {		    
			    edgeWriter.println(edges[i].getFromNode().getId()
					       + " " + edges[i].getToNode().getId());
			}
		    }

		    labelWriter.println(n.getSeq());		    
		}
	    
		edgeWriter.close();
		labelWriter.close();

	    } catch (IOException e) {
		e.printStackTrace();
	    }
	}    
    }

    /**
     * This is also important, as it will be used as input to genevo. Because 
     * the graphs puts different numbers on the nodes (by iterating through 
     * the hashmap), we need to use the ids to be able to compare edges with 
     * gedevo.
     **/
    public void gedevoInput(String graphName, PrintWriter writer) {

	for (Node n : nodeList.values()) {
	    Edge[] edges = n.getEdges();

	    if (edges != null) {
		for (int i = 0; i < edges.length; i++) {		    
		    writer.println(graphName 
				   + " " + edges[i].getFromNode().getId() + 
				   " "  + edges[i].getToNode().getId());
		}							
	    }
	}
    }
}

/**
 * Edges are attributes of the nodes and contains a start node and an end node 
 *
 **/
class Edge {
  
    private Node to;
    private Node from;  
    
    Edge(Node from, Node to) {
	this.from = from;
	this.to = to;
    }

    public Node getToNode() {
	return to;
    }

    public Node getFromNode() {
	return from;
    }
}

/**
 * Nodes contains edges, identifiers and a sequence. The graph is a HashMap of 
 * nodes.
 **/
class Node {
    private int pos;
    private String id;
    private String seq;
    private String type;
    private int nodeNumber;
    
    private Edge[] edges;

    Node(int pos, String id, String seq, String type) {
	this.pos = pos;
	this.id = id;
	this.seq = seq;
	this.type = type;
    }

    // A lot of get/sets
    public void setEdges(Edge[] edges) {
	this.edges = edges;
    }

    public Edge[] getEdges() {
	return edges;
    }

    public String getId() {
	return id;
    }

    public void setNumber(int nodeNumber) {
	this.nodeNumber = nodeNumber;
    }

    public int getNodeNumber() {
	return nodeNumber;
    }

    public String getSeq() {
	return seq;
    }
    
    public void setSeq(String seq) {
	this.seq = seq;
    }

    public int getPos() {
	return pos;
    }

    public String getType() {
	return type;
    }

    /**
     * If there's more than two edges out from an in-between node, we'll need 
     * to add another if more variants appear at the same position.
     **/
    public void addEdge(Node edgeFrom, Node edgeTo) { 
	Edge[] update = new Edge[edges.length + 1];

	for (int i = 0; i < edges.length; i++) {
	    update[i] = edges[i];
	}

	update[update.length - 1] = new Edge(edgeFrom, edgeTo);
	setEdges(update);
    }

    public void display() {
	System.out.println("Node " + id + " of type " + type + 
			   " and igraph-numering " + nodeNumber + 
			   " reporting: My seq is " + seq + 
			   " at position " + pos + 
			   ".\nMy edges are from myself to ");
	
	if (edges != null) {
	    for (int i = 0; i < edges.length; i++) {
		if (edges[i] == null) {
		    System.out.println("Warning: Edge is null");
		} else {
		    System.out.println(edges[i].getToNode().getId());
		}
	    }
	    
	} else {
	    System.out.println("<None>");
	}
	System.out.println();
    }
}



