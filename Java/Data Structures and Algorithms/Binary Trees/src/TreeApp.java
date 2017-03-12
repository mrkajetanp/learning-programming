import java.util.*; // for Stack class

class TreeNode {
    public int iData;
    public double dData;
    public TreeNode leftChild;
    public TreeNode rightChild;

    public void display() {
        System.out.print("{");
        System.out.print(iData);
        System.out.print(", ");
        System.out.print(dData);
        System.out.print("} ");
    }
}

class BinaryTree {
    private TreeNode root;

    public BinaryTree () {
        root = null;
    }

    public TreeNode find(int key) {
        TreeNode current = root;
        while (current.iData != key) {
            if (key < current.iData) // go left?
                current = current.leftChild;
            else
                current = current.rightChild;
            if (current == null) // if no child, didn't find it
                return null;
        }
        return current;
    }

    public void insert(int id, double dd) {
        TreeNode newNode = new TreeNode();
        newNode.iData = id; // insert data
        newNode.dData = dd;
        if (root == null)
            root = newNode;
        else {
            TreeNode current = root; // start at root
            TreeNode parent;
            while (true) {
                parent = current;
                if (id < current.iData) { // go left?
                    current = current.leftChild;
                    if (current == null) { // if end of the line, insert on left
                        parent.leftChild = newNode;
                        return;
                    }
                }
                else { // or go right?
                    current = current.rightChild;
                    if (current == null) {
                        parent.rightChild = newNode;
                        return;
                    }
                }
            }
        }
    }

    public boolean delete(int key) { // assumes non-empty list
        TreeNode current = root;
        TreeNode parent = root;
        boolean isLeftChild = true;

        while (current.iData != key) { // search for the node
            parent = current;
            if (key < current.iData) { // go left?
                isLeftChild = true;
                current = current.leftChild;
            }
            else { // or go right?
                isLeftChild = false;
                current = current.rightChild;
            }
            if (current == null)
                return false;
        }
        // found the node to delete

        // if no children, just delete it
        if (current.leftChild == null && current.rightChild == null) {
            if (current == root) // if root
                root = null; // tree is empty
            else if (isLeftChild)
                parent.leftChild = null;
            else
                parent.rightChild = null;
        }

        // if no right child, replace with left subtree
        else if (current.rightChild == null) {
            if (current == root)
                root = current.leftChild;
            else if (isLeftChild)
                parent.leftChild = current.leftChild;
            else
                parent.rightChild = current.leftChild;
        }

        // if no left child, replace with right subtree
        else if (current.leftChild == null) {
            if (current == root)
                root = current.rightChild;
            else if (isLeftChild)
                parent.leftChild = current.rightChild;
            else
                parent.rightChild = current.rightChild;
        }

        // two children, replace with inorder successor
        else {
            // get successor of node to delete (current)
            TreeNode successor = getSuccessor(current);

            // connect parent to succossor instead
            if (current == root)
                root = successor;
            else if (isLeftChild)
                parent.leftChild = successor;
            else
                parent.rightChild = successor;

            // connect successor to current's left child
            successor.leftChild = current.leftChild;
        }
        return true;
    }

    private TreeNode getSuccessor(TreeNode delNode) {
        TreeNode successorParent = delNode;
        TreeNode successor = delNode;
        TreeNode current = delNode.rightChild;
        while (current != null) {
            successorParent = successor;
            successor = current;
            current = current.leftChild;
        }

        // if successor is not right child, make connections
        if (successor != delNode.rightChild) {
            successorParent.leftChild = successor.rightChild;
            successor.rightChild = delNode.rightChild;
        }
        return successor;
    }

    public void traverse(int traverseType) {
        switch (traverseType) {
        case 1:
            System.out.print("\nPreorder traversal: ");
            preOrder(root);
            break;
        case 2:
            System.out.print("\nInorder traversal: ");
            inOrder(root);
            break;
        case 3:
            System.out.print("\nPostorder traversal: ");
            postOrder(root);
            break;
        }
        System.out.println();
    }

    private void preOrder(TreeNode localRoot) {
        if (localRoot != null) {
            System.out.println(localRoot.iData + " ");
            preOrder(localRoot.leftChild);
            preOrder(localRoot.rightChild);
        }
    }

    private void inOrder(TreeNode localRoot) {
        if (localRoot != null) {
            inOrder(localRoot.leftChild);
            System.out.println(localRoot.iData + " ");
            inOrder(localRoot.rightChild);
        }
    }

    private void postOrder(TreeNode localRoot) {
        if (localRoot != null) {
            postOrder(localRoot.leftChild);
            postOrder(localRoot.rightChild);
            System.out.println(localRoot.iData + " ");
        }
    }

    public void display() {
        Stack<TreeNode> globalStack = new Stack<TreeNode>();
        globalStack.push(root);
        int nBlanks = 32;
        boolean isRowEmpty = false;
        System.out.println(".........................................................");
        while (isRowEmpty == false) {
            Stack<TreeNode> localStack = new Stack<TreeNode>();
            isRowEmpty = true;

            for (int i = 0 ; i  < nBlanks ; ++i)
                System.out.print(' ');

            while(globalStack.isEmpty() == false) {
                TreeNode temp = (TreeNode)globalStack.pop();
                if (temp != null) {
                    System.out.print(temp.iData);
                    localStack.push(temp.leftChild);
                    localStack.push(temp.rightChild);

                    if (temp.leftChild != null || temp.rightChild != null)
                        isRowEmpty = false;
                }
                else {
                    System.out.print("--");
                    localStack.push(null);
                    localStack.push(null);
                }
                for (int i = 0 ; i < nBlanks*2-2 ; ++i)
                    System.out.print(" ");
            }
            System.out.println("");
            nBlanks /= 2;
            while (localStack.isEmpty() == false)
                globalStack.push(localStack.pop());
        }
        System.out.println(".........................................................");
    }
}

public class TreeApp {
    public static void main(String[] args) {
        BinaryTree theTree = new BinaryTree();
        theTree.insert(50, 1.5);
        theTree.insert(25, 1.2);
        theTree.insert(64, 1.7);
        theTree.insert(75, 1.3);
        theTree.insert(24, 1.1);
        theTree.insert(55, 1.4);
        theTree.insert(12, 1.8);
        theTree.insert(23, 1.9);
        theTree.insert(67, 1.0);
        theTree.insert(44, 1.0);
        theTree.insert(45, 1.0);
        theTree.insert(43, 1.0);
        theTree.insert(12, 1.0);
        theTree.insert(86, 1.6);
        theTree.insert(56, 1.6);
        theTree.insert(95, 1.1);
        theTree.insert(77, 1.5);

        theTree.display();
    }
}
