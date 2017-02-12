#include <iostream>

typedef int listItem;

class DNode {
    private:
        listItem item;
        DNode* prev;
        DNode* next;
        friend class DLinkedList;
};

class DLinkedList {
    public:
        DLinkedList();
        ~DLinkedList();
        bool isEmpty() const;
        const listItem& front () const;
        const listItem& back () const;
        void insertFront (const listItem& x);
        void insertBack (const listItem& x);
        void removeFront ();
        void removeBack ();
    private:
        DNode* header;
        DNode* trailer;
    protected:
        void remove (DNode* v);
};

DLinkedList::DLinkedList() {
    header = new DNode;
    trailer = new DNode;
    header->next = trailer;
    trailer->prev = header;
}

DLinkedList::~DLinkedList() {
    while (!isEmpty())    removeFront();
    delete header;
    delete trailer;
}

bool DLinkedList::isEmpty () const {
    return (header->next == trailer);
}

const listItem& DLinkedList::front () const {
    return header->next->item;
}

const listItem& DLinkedList::back () const {
    return trailer->prev->item;
}

int main () {
    return 0;
}
