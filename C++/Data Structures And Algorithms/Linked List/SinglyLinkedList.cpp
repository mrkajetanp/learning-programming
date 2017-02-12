#include <iostream>

using std::cout;
using std::endl;

template <typename T>
class SLinkedList;

template <typename T>
class SNode {
    public:
        T item ();
        SNode<T>* next ();
    private:
        T _item;
        SNode<T>* _next;
        friend class SLinkedList<T>;
};

template <typename T>
T SNode<T>::item () {
    return _item;
}

template <typename T>
SNode<T>* SNode<T>::next () {
    return _next;
}

template <typename T>
class SLinkedList {
    public:
        SLinkedList ();
        ~SLinkedList ();
        bool isEmpty () const;
        const T& front () const;
        void add (const T& x);
        void removeFront ();
        void printList ();
        SNode<T>* searchList (const T& x);
    private:
        SNode<T>* head;
};

template <typename T>
SLinkedList<T>::SLinkedList() : head(NULL) { }

template <typename T>
bool SLinkedList<T>::isEmpty() const {
    return head == NULL;
}

template <typename T>
const T& SLinkedList<T>::front() const {
    return head->_item;
}

template <typename T>
SLinkedList<T>::~SLinkedList() {
    while (!isEmpty())
        removeFront();
}

template <typename T>
void SLinkedList<T>::add (const T& x) {
    SNode<T>* v = new SNode<T>;
    v->_item = x;
    v->_next = head;
    head = v;
}

template <typename T>
void SLinkedList<T>::removeFront () {
    SNode<T>* old = head;
    head = old->_next;
    delete old;
}

template <typename T>
void SLinkedList<T>::printList () {
    SNode<T>* node = head;
    while (node != NULL) {
        cout << node->_item << " ";
        node = node->_next;
    }
    cout << endl;
}

template <typename T>
SNode<T>* SLinkedList<T>::searchList (const T& x) {
    SNode<T>* node = head;
    while (node != NULL) {
        if (node->_item == x)
            return node;
        else
            node = node->_next;
    }
    return NULL;
}

int main () {
    SLinkedList<int> listOne;
    listOne.add (18);
    listOne.add (25);
    listOne.add (33);
    listOne.printList ();
    cout << listOne.searchList(25)->next()->item() << endl;
    listOne.removeFront ();
    listOne.printList ();

    return 0;
}
