/**
 * ANALYSES FOR MASTER'S THESIS
 * ============================================================================
 *
 * GedevoGraph.java
 * @author Martin Gjesdal Bjørndal
 * 
 * This program builds a graph from a VCF-file with intermediary nodes 
 * in-between the variants. Each variant makes a "fork" and two variants at the 
 * same position makes a three-way fork because all variants at the same 
 * position has the same referende, so this is seemingly done only to not have 
 * lists in the ALT-column in the VCF-file.
 **/

import java.util.Scanner;
import java.util.ArrayList;
import java.util.HashMap;

import java.io.File;
import java.io.PrintWriter;
import java.io.FileNotFoundException;
import java.io.IOException;


public class GedevoGraph {
    public static void main(String[] args) {
	TestClass t = new TestClass();
	t.test(args[0], args[1]);
    }
}

class TestClass {
    public void test(String filename1, String filename2) {
	File infile1 = new File(filename1);
	File infile2 = new File(filename2);

	HashMap<String, Node> nodeList1; 
	HashMap<String, Node> nodeList2; 

	FileReader reader1 = new FileReader();
	FileReader reader2 = new FileReader();

	nodeList1 = reader1.readFile(infile1);	
	nodeList2 = reader2.readFile(infile2);

	Graph g1 = new Graph(nodeList1);
	Graph g2 = new Graph(nodeList2);

	g1.setNodeNumbers();	
	g2.setNodeNumbers();      

	try {
	    PrintWriter writer = new PrintWriter("gedevoInput.txt", "UTF-8");
	    writer.println("graph fromNode toNode");
	    g1.gedevoInput("g1", writer);
	    g2.gedevoInput("g2", writer);
		 
	    writer.close();
   
	} catch (IOException e) {
	    e.printStackTrace();
	}
    }
}

class FileReader {

    /**
     *  A lot of things actually happens here. The R-preprocessing removed the
     * lines that specified nonextistent variations. So all lines now 
     * translates to two nodes. Must draw edges to the node at the "fork" point.
     * And we need to do some more complex things where variations happen close 
     * by or on top of each other.
     * 
     * @param infile the VCF-file to be read
     * @return nodeList HashMap of nodes eventually passed to the constructor 
     * in Graph.java
     **/
    public HashMap<String, Node> readFile(File infile) {
	
	HashMap<String, Node> nodeList = new HashMap<String, Node>();

	try {
	    Scanner scan = new Scanner(infile);
	    scan.nextLine();

	    // The last nodes needs to be readily at hand
	    Node lastInBetween = null;
	    Node previousRef = null;
	    Node previousAlt = null;
	    ArrayList<Node> previousVariants = new ArrayList<Node>();

	    int previousPos = 0;
	    int lineCounter = 0;
	    while (scan.hasNext()) {
		String line = scan.nextLine();
		String[] lineList = line.split(" ");

		// All lines specify two nodes: One for the reference type and
		// one for the alternative. The reference is column no. 4, the
		// alternative in no. 5.
		int pos = Integer.parseInt(lineList[2]);
		Node alt = new Node(pos, lineList[3].concat("alt"), 
				    lineList[5], "alt");
		
		// The most interesting thing is when there are multiple 
		// variations at the same position! This is really the same as
		// a multiple-way fork. So we can just add on a connection from
		// the last in-between-node.
		if (pos == previousPos) {
		    nodeList.get(lastInBetween.getId()).addEdge(lastInBetween, alt);
		    nodeList.put(alt.getId(), alt);
		    previousVariants.add(alt);
				   
		} else if (pos == previousPos + 1) {
		    Node ref = new Node(pos, lineList[3].concat("ref"), 
					lineList[4], "ref");

		    // Need to go two steps back to look at how many nodes there 
		    // were in the previous step
		    Edge[] edges = new Edge[2];
		    edges[0] = new Edge(lastInBetween, ref);
		    edges[1] = new Edge(lastInBetween, alt);

		    Edge[] edgesToPreviousStep = lastInBetween.getEdges();
		    int noOfNodesPreviousStep = edgesToPreviousStep.length;
		    Node[] nodesPreviousStep = new Node[noOfNodesPreviousStep];

		    for (int i = 0; i < edgesToPreviousStep.length; i++) {
			nodesPreviousStep[i] = edgesToPreviousStep[i].getToNode();
		    }

		    Edge[] edgesFromPreviousStep = new Edge[noOfNodesPreviousStep];
		    
		    // And now we must set the edges in this step. Overwrite 
		    // the variable edges
		    for (int i = 0; i < nodesPreviousStep.length; i++) {
			edges = new Edge[2];
			edges[0] = new Edge(nodesPreviousStep[i] , ref);
			edges[1] = new Edge(nodesPreviousStep[i], alt);
			nodesPreviousStep[i].setEdges(edges);
			    
		    }
		    
		    nodeList.put(ref.getId(), ref);
		    nodeList.put(alt.getId(), alt);
		    previousRef = ref;

		    previousVariants.clear();
		    previousVariants.add(ref);
		    previousVariants.add(alt);

		} else  {		    
		    Node ref = new Node(pos, lineList[3].concat("ref"), 
					lineList[4], "ref");

		    // If we ain't stacking variations then this is just another 
		    // SNP.
		    if (lineCounter == 0) {
			lastInBetween = new Node(1, "Start", "Start", "start");
			Edge[] edges = new Edge[2];
			edges[0] = new Edge(lastInBetween, ref);
			edges[1] = new Edge(lastInBetween, alt);
			lastInBetween.setEdges(edges);

			nodeList.put(lastInBetween.getId(), lastInBetween);				
			nodeList.put(ref.getId(), ref);
			nodeList.put(alt.getId(), alt);

		    } else {		      
			lastInBetween = new Node(pos - 1, 
						 lineList[3].concat("inter"), 
						 "I", "inter");
			Edge[] edges = new Edge[2];
			edges[0] = new Edge(lastInBetween, ref);
			edges[1] = new Edge(lastInBetween, alt);			
			lastInBetween.setEdges(edges);
					    
			// Set the edges from two steps ago to the in-between node.			
			previousRef = nodeList.get(previousRef.getId());
			previousAlt = nodeList.get(previousAlt.getId());

			for (Node n : previousVariants) {
			    Edge[] edgeToInBetween = new Edge[1];
			    edgeToInBetween[0] = new Edge(n, lastInBetween);
			    nodeList.get(n.getId()).setEdges(edgeToInBetween);
			}

			nodeList.put(lastInBetween.getId(), lastInBetween);
		    }

		    nodeList.put(ref.getId(), ref);
		    nodeList.put(alt.getId(), alt);
		    
		    previousVariants.clear();

		    previousRef = ref;
		    previousVariants.add(ref);
		} 
		
		previousPos = pos;
		previousAlt = alt;
		previousVariants.add(alt);
		//System.out.println(previousVariants.size());
		lineCounter++;	
	    }
	} catch (FileNotFoundException e) {
	    e.printStackTrace();
	}

	System.out.println("\nFile read");
	return nodeList;
    }
}
