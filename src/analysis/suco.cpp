#ifndef SUCO_CPP
#define SUCO_CPP

//
// The data structures used by SUan and COlby (SUCO) in this
// project.  It includes:
//   suco_llist - a linked list
//   suco_queue - a queue
//   suco_stack - a stack
//   suco_olist - an ordered list
//   suco_set   - a set
// Note: very little 'protection' is provided; designed on the
// assumption that its users (us) know what they (we) are doing.
//

#include "suco.h"

//----------SUCO_NODE-----------
template <class T>
typename suco_llist<T>::_suco_node * suco_llist<T>::_suco_node::freelist = 0;

template <class T>
int suco_llist<T>::_suco_node::num_new_nodes = 0;
template <class T>
int suco_llist<T>::_suco_node::num_recy_nodes = 0;

template <class T>
typename suco_llist<T>::_suco_node * suco_llist<T>::_suco_node::acquire(T i, _suco_node * n)
{
  if(freelist){
    num_recy_nodes++;
    _suco_node * nn = freelist;
    freelist = freelist->next;
    nn->item = i;
    nn->next = n;
    return nn;
  } else {
    num_new_nodes++;
    return new _suco_node(i,n);
  }

}

template <class T>
void suco_llist<T>::_suco_node::dispose(_suco_node * n)
{
  if(suco_recycle){
    n->item = 0;
    n->next = freelist;
    freelist = n;
  } else {
    delete n;
  }
}

template <class T>
int suco_llist<T>::_suco_node::freelistLength()
{
  int i = 0;
  for(_suco_node * n = freelist; n; n = n->next) i++;
  return i;
}

//----------SUCO_LLIST-----------

template <class T>
suco_llist<T>::~suco_llist()
{
  _suco_node * deleteval;
  while(_head){
    deleteval = _head;
    _head = _head->next;
    _suco_node::dispose(deleteval);
  }
}

template <class T>
bool suco_llist<T>::Insert(T item)
{
  if(!item) return false;

  _head = _suco_node::acquire(item, _head);
  if(!_head->next) _tail = &(_head->next);
  return true;
}

template <class T>
void suco_llist<T>::Append(T item)
{
  if(!item) return;

  *_tail = _suco_node::acquire(item);
  _tail = &((*_tail)->next);
}

template <class T>
void suco_llist<T>::Attach(suco_llist<T>& list)
{
  if(list._head){
    *_tail = list._head;
    _tail = list._tail;
    list._head = 0;
    list._tail = &list._head;
  }
}

template <class T>
void suco_llist<T>::Copy(suco_llist<T>& list)
{
  if(list._head){
    Clear();

    _suco_node ** nn = &_head;

    for(_suco_node * ln = list._head; ln; ln = ln->next){
      *nn = _suco_node::acquire(ln->item);
      nn = &(*nn)->next;
    }

    _tail = nn;
  }
}


template <class T>
T suco_llist<T>::Last()
{
  T last = 0;
  suco_iterator<T> si(*this);
  while(si.Iterate())
    last = si.Current();
  return last;
}

template <class T>
T suco_llist<T>::ElementAt(int index)
{
  suco_iterator<T> si(*this);
  int i = 0;
  while(si.Iterate()){
    if(++i >= index)
      return si.Current();
  }
  return 0;
}

template <class T>
bool suco_llist<T>::Remove(T item)
{
  suco_iterator<T> si(*this);
  while(si.Iterate()){
    if(si.Current() == item){
      si.DeleteCurrent();
      return true; //- found, removed
    }
  }
  return false; //- not found
}

template <class T>
T suco_llist<T>::RemoveHead()
{
  if(!_head) return 0;

  T returnval = _head->item;
  _suco_node * deleteval = _head;
  _head = _head->next;
  if(!_head) _tail = &_head;

  _suco_node::dispose(deleteval);
  return returnval;
}

template <class T>
void suco_llist<T>::Clear()
{
  suco_iterator<T> si(*this);
  while(si.Iterate())
    si.DeleteCurrent();
}

template <class T>
bool suco_llist<T>::Contains(T item)
{
  suco_iterator<T> si(*this);
  while(si.Iterate())
    if(si.Current() == item)
      return true;
  
  return false;
}

template <class T>
int suco_llist<T>::Length()
{
  int returnval = 0;
  suco_iterator<T> si(*this);
  while(si.Iterate()) returnval++;
  return returnval;
}

//----------SUCO_QUEUE-----------

template <class T>
void suco_queue<T>::Enqueue(T item)
{
  Append(item);
}

template <class T>
T suco_queue<T>::Dequeue()
{
  return RemoveHead();
}

//----------SUCO_STACK-----------

template <class T>
void suco_stack<T>::Push(T item)
{
  Insert(item);
}

template <class T>
T suco_stack<T>::Pop()
{
  return RemoveHead();
}

template <class T>
suco_stack<T>::operator bool()
{
  return !IsEmpty();
}

//----------SUCO_OLIST-----------

template <class T>
bool suco_olist<T>::Insert(T item)
{
  if(!item) return false;

  typename suco_olist<T>::_suco_node ** i;

  for(i = &_head; *i; i = &((*i)->next)){
    if(compare(item, (*i)->item) < 0){
      *i = _suco_node::acquire(item, *i);
      return true;
    }
  }
  Append(item);
  return true;
}

template <class T>
bool suco_olist<T>::Contains(T item)
{
  suco_iterator<T> si(*this);
  while(si.Iterate()){
    if(!compare(si.Current(), item))
      return true;
    //- TODO: can optimize by breaking out early if <
  }
  
  return false;
}

//----------SUCO_SET-----------

template <class T>
suco_set<T>::suco_set(suco_set<T>& src, int (*cf)(T,T))
: suco_olist<T>(cf)
{
  suco_iterator<T> si(src);

  while(si.Iterate())
    Insert(si.Current());
  
}

template <class T>
bool suco_set<T>::Insert(T item)
{
  if(!item) return false;
  typename suco_set<T>::_suco_node ** i;

  for(i = &_head; *i; i = &((*i)->next)){
    int cf = compare(item, (*i)->item);
    if(cf == 0){ //- item found
      return false;
    } else if(cf < 0){
      *i = _suco_node::acquire(item, *i);
      return true;
    }
  }
  Append(item);
  return true;
}

template <class T>
T suco_set<T>::GetSingleton()
{
  if(_head && !_head->next) return _head->item;
  else return 0;
}

template <class T>
void suco_set<T>::InsertList(suco_llist<T> &tgt)
{
  suco_iterator<T> ti(tgt);
  while(ti.Iterate())
    this->Insert(ti.Current());
}

// - creates a copy of added elements
template <class T>
void suco_set<T>::Union(suco_set<T> &set)
{
  typename suco_set<T>::_suco_node ** tp = &this->_head;
  typename suco_set<T>::_suco_node * sp = set._head;

  while(sp){
    if(!*tp){
      *tp = _suco_node::acquire(sp->item);
      tp = &(*tp)->next;
      sp = sp->next;
    } else {
      int cf = compare((*tp)->item, sp->item);
      if(cf < 0){
        tp = &(*tp)->next;
      } else if(cf == 0){
        tp = &(*tp)->next;
        sp = sp->next;
      } else {
        *tp = _suco_node::acquire(sp->item, *tp);
        tp = &(*tp)->next;
        sp = sp->next;
      }
    }
  }
  if(!*tp)
    this->_tail = tp;
}

// - removes elements from set
template <class T>
void suco_set<T>::UnionConsume(suco_set<T> &set)
{
  typename suco_set<T>::_suco_node ** tp = &this->_head;
  typename suco_set<T>::_suco_node * sp = set._head;
  set._head = 0; // - clear set

  while(sp){
    if(!*tp){
      *tp = sp;
      this->_tail = set._tail;
      set._tail = &set._head;
      return;
    } else {
      int cf = compare((*tp)->item, sp->item);
      if(cf < 0){
        tp = &(*tp)->next;
      } else if(cf == 0){
        typename suco_set<T>::_suco_node * tmp = sp;
        sp = sp->next;
        _suco_node::dispose(tmp);
        tp = &(*tp)->next;
      } else {
        typename suco_set<T>::_suco_node * tmp = sp;
        sp = sp->next;
        tmp->next = *tp;
        *tp = tmp;
        tp = &(*tp)->next;
      }
    }
  }
  set._tail = &set._head;
}

template <class T>
bool suco_set<T>::Subset(suco_set<T> &tgt)
{
//--SUBOPTIMAL: REWRITE
  suco_iterator<T> si(*this);

  while(si.Iterate())
    if(!tgt.Contains(si.Current()))
      return false;

  return true;
}

template <class T>
bool suco_set<T>::Superset(suco_set<T> &tgt)
{
//--SUBOPTIMAL: REWRITE
  suco_iterator<T> si(tgt);

  while(si.Iterate())
    if(!this->Contains(si.Current()))
      return false;

  return true;
}

template <class T>
void suco_set<T>::Intersect(suco_set<T> &tgt)
{
//--SUBOPTIMAL: REWRITE
  suco_iterator<T> ti(*this);

  while(ti.Iterate())
    if(!tgt.Contains(ti.Current()))
      ti.DeleteCurrent();
}

template <class T>
void suco_set<T>::Subtract(suco_set<T> &tgt)
{
//--SUBOPTIMAL: REWRITE
  suco_iterator<T> ti(*this);

  while(ti.Iterate())
    if(tgt.Contains(ti.Current()))
      ti.DeleteCurrent();
}

template <class T>
bool suco_set<T>::Intersects(suco_llist<T> &tgt)
{
//--SUBOPTIMAL: REWRITE
  suco_iterator<T> si(tgt);

  while(si.Iterate())
    if(this->Contains(si.Current()))
      return true;

  return false;
}

//--------SUCO_ITERATOR---------

template <class T>
bool suco_iterator<T>::Iterate()
{
  if(!_valid) _valid = true;
  else if(*_ptr) _ptr = &((*_ptr)->next);
  return ((*_ptr)!=0);
}

template <class T>
void suco_iterator<T>::DeleteCurrent()
{
  if(*_ptr){
    typename suco_set<T>::_suco_node * deleteval = *_ptr;
    *_ptr = deleteval->next;
    if(!(*_ptr)) *_tail = _ptr;
    suco_llist<T>::_suco_node::dispose(deleteval);
    _valid = false;
  }
}

#endif
