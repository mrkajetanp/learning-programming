#include "Tree.h"
#include <stdio.h>
#include <stdlib.h>

void treeInsertNode(TreeNode** root, COMPARE compare, void* item) {
    TreeNode* node = malloc(sizeof *node);
    node->item = item;
    node->left = NULL;
    node->right = NULL;

    if (*root == NULL) { // the tree is empty
        *root = node;
        return;
    }

    while (1) {
        if (compare((*root)->item, item) > 0) {
            if ((*root)->left != NULL)
                *root = (*root)->left;
            else {
                (*root)->left = node;
                break;
            }
        }
        else {
            if ((*root)->right != NULL)
                *root = (*root)->right;
            else {
                (*root)->right = node;
                break;
            }
        }
    }

}

void treeTraverseInOrder(TreeNode* root, DISPLAY display) {
    if (root != NULL) {
        treeTraverseInOrder(root->left, display);
        display(root->item);
        treeTraverseInOrder(root->right, display);
    }
}

void treeTraversePostOrder(TreeNode* root, DISPLAY display) {
    if (root != NULL) {
        treeTraversePostOrder(root->left, display);
        treeTraversePostOrder(root->right, display);
        display(root->item);
    }
}

void treeTraversePreOrder(TreeNode* root, DISPLAY display) {
    if (root != NULL) {
        display(root->item);
        treeTraversePreOrder(root->left, display);
        treeTraversePreOrder(root->right, display);
    }
}
