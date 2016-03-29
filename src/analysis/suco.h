#ifndef SUCO_H
#define SUCO_H

//
// The data structures used by SUan and COlby (SUCO) in this
// project.  It includes:
//   suco_llist - a linked list
//   suco_queue - a queue
//   suco_olist - an ordered list
//   suco_set   - a set
//   suco_iterator - an iterator that can be used in conjunction
//                   with any of the above data structures.
// Note: very little 'protection' is provided; designed on the
// assumption that its users (us) know what they (we) are doing.
//

//
// CAVEATS:
//  these are designed to hold pointers only; further, Insert() will
//  discard null pointers (for convenience)
//

#include <stdio.h>

//-- these are defined in suco_usage.cpp
extern bool suco_recycle;

class suco_diag_wrapper
{
  public: static void suco_write_recycle_stats(FILE * outf);
};

template <class T> class suco_iterator;

//
// A linked list.
//
template <class T>
class suco_llist
{
  public:
    suco_llist(): _head(0), _tail(&_head){}
    ~suco_llist();

    bool Contains(T item);

    bool Insert(T item); // return true if success (mainly for suco_set)

    void Append(T item);

    void Attach(suco_llist<T>& list);

    void Copy(suco_llist<T>& list);

    T Head() { return (_head != 0)?(_head->item):0; }

    T Last();

    T ElementAt(int index);

    bool Remove(T item); // true if item found and removed

    T RemoveHead();

    void Clear();

    int Length();

    bool IsEmpty() { return (_head==0); }

    friend class suco_iterator<T>;
    friend class suco_diag_wrapper;

  protected:

    class _suco_node
    {
      public:
        static _suco_node * acquire(T i, _suco_node * n = 0);
        static void dispose(_suco_node * n);
        static int freelistLength();
        static int num_new_nodes;
        static int num_recy_nodes;

        T item;
        _suco_node * next;

      private:
        _suco_node();
        _suco_node(T i, _suco_node * n)
                  :item(i),next(n) {}
        ~_suco_node() {}
        static _suco_node * freelist;
    };

    _suco_node * _head;

    _suco_node ** _tail;
};

//
// A queue
//
template <class T>
class suco_queue:public suco_llist<T>
{
  public:
    suco_queue() {}

    void Enqueue(T item);

    T Dequeue();

  private:
};

//
// A stack
//
template <class T>
class suco_stack:public suco_llist<T>
{
  public:
    suco_stack() {}

    void Push(T item);

    T Pop();

    operator bool(); // !isEmpty()

  private:
};

//
// An ordered list: instantiated with a compare function that
// is used to determine the position in the list in which to
// insert a new element.
//
template <class T>
class suco_olist:public suco_llist<T>
{
  public:
    suco_olist(int (*cf)(T,T) = default_compare):compare(cf) {}

    bool Insert(T item); // return true if success (mainly for suco_set)
    bool Contains(T item);

  protected:
    int (*compare)(T,T);
    static int default_compare(T l,T r) { return ((int)l - (int)r); }
};

//
// A set class.  The only thing 'unusual' would be that
// the set cannot contain any 'NULL' elements (0).  This
// property makes coding for us a little more convenient.
//
template <class T>
class suco_set:public suco_olist<T>
{
  public:
    suco_set(int (*cf)(T,T) = suco_olist<T>::default_compare) : suco_olist<T>(cf) {}
    suco_set(suco_set<T> & src, int (*cf)(T,T) = suco_olist<T>::default_compare);

    bool Insert(T); // return true if success

    T GetSingleton();

    void InsertList(suco_llist<T> &tgt);

    void Union(suco_set<T> &tgt);

    void UnionConsume(suco_set<T> &tgt);

    bool Superset(suco_set<T> &tgt);

    bool Subset(suco_set<T> &tgt);

    void Intersect(suco_set<T> &tgt);

    void Subtract(suco_set<T> &tgt);

    bool Intersects(suco_llist<T> &tgt);
};

//
// An iterator for all the above classes (which are all derived
// from the llist, BTW).
// The use of this is as follows:
//
//  suco_iterator<type> name(list);
//  while(name.Iterate()){
//    use name.Current();
//    [name.DeleteCurrent();]
//  }
//
template <class T>
class suco_iterator
{
  public:
    suco_iterator(suco_llist<T> &s)
               : _list(&(s._head)), _tail(&(s._tail)),
                 _ptr(&(s._head)), _valid(false) {}
    suco_iterator(suco_iterator<T> &i)	//- copy constructor: sets list to current ptr!
               : _list(i._ptr), _tail(i._tail),
                 _ptr(i._ptr), _valid(i._valid) {}
    T Current() { return (_valid && (*_ptr)!=0)?((*_ptr)->item):0; }
    void DeleteCurrent();
    bool Iterate();
    operator bool(){ return (_valid && ((*_ptr)!=0)); }
    void Rewind() { _ptr = _list; _valid = false; }

  protected:
    suco_iterator();
    typename suco_llist<T>::_suco_node ** _list, *** _tail, ** _ptr;
    bool _valid;
};

#endif
