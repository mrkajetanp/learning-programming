#ifndef TREE_H
#define TREE_H

typedef int(*COMPARE)(void*, void*);
typedef void(*DISPLAY)(void*);

typedef struct _treeNode {
    void *item;
    struct _treeNode* left;
    struct _treeNode* right;
} TreeNode;

void treeInsertNode(TreeNode** root, COMPARE compare, void* item);
void treeTraverseInOrder(TreeNode* root, DISPLAY display);
void treeTraversePostOrder(TreeNode* root, DISPLAY display);
void treeTraversePreOrder(TreeNode* root, DISPLAY display);

#endif
