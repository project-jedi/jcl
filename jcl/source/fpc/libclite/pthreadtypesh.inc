
type
  P_pthread_fastlock = ^_pthread_fastlock;
  _pthread_fastlock = record
    __status : longint;
    __spinlock : longint;
  end;

  P_pthread_descr = ^_pthread_descr;
  _pthread_descr = pointer; // Opaque type.

  P__pthread_attr_s = ^__pthread_attr_s;
  __pthread_attr_s = record
       __detachstate : longint;
       __schedpolicy : longint;
       __schedparam : __sched_param;
       __inheritsched : longint;
       __scope : longint;
       __guardsize : size_t;
       __stackaddr_set : longint;
       __stackaddr : pointer;
       __stacksize : size_t;
    end;
  pthread_attr_t = __pthread_attr_s;
  Ppthread_attr_t = ^pthread_attr_t;

  Ppthread_cond_t = ^pthread_cond_t;
  pthread_cond_t = unixtype.pthread_cond_t;
  {record
       __c_lock : _pthread_fastlock;
       __c_waiting : _pthread_descr;
    end;}

  Ppthread_condattr_t = ^pthread_condattr_t;
  pthread_condattr_t = record
       __dummy : longint;
    end;

  Ppthread_key_t = ^pthread_key_t;
  pthread_key_t = dword;

  Ppthread_mutex_t = ^pthread_mutex_t;
  pthread_mutex_t = record
       __m_reserved : longint;
       __m_count : longint;
       __m_owner : _pthread_descr;
       __m_kind : longint;
       __m_lock : _pthread_fastlock;
    end;

  Ppthread_mutexattr_t = ^pthread_mutexattr_t;
  pthread_mutexattr_t = record
       __mutexkind : longint;
    end;

  Ppthread_once_t = ^pthread_once_t;
  pthread_once_t = longint;

  P_pthread_rwlock_t = ^_pthread_rwlock_t;
  _pthread_rwlock_t = record
       __rw_lock : _pthread_fastlock;
       __rw_readers : longint;
       __rw_writer : _pthread_descr;
       __rw_read_waiting : _pthread_descr;
       __rw_write_waiting : _pthread_descr;
       __rw_kind : longint;
       __rw_pshared : longint;
    end;
  pthread_rwlock_t = _pthread_rwlock_t;
  Ppthread_rwlock_t = ^pthread_rwlock_t;

  Ppthread_rwlockattr_t = ^pthread_rwlockattr_t;
  pthread_rwlockattr_t = record
       __lockkind : longint;
       __pshared : longint;
    end;

  Ppthread_spinlock_t = ^pthread_spinlock_t;
  pthread_spinlock_t = longint;

  Ppthread_barrier_t = ^pthread_barrier_t;
  pthread_barrier_t = record
       __ba_lock : _pthread_fastlock;
       __ba_required : longint;
       __ba_present : longint;
       __ba_waiting : _pthread_descr;
    end;

  Ppthread_barrierattr_t = ^pthread_barrierattr_t;
  pthread_barrierattr_t = record
       __pshared : longint;
    end;

  Ppthread_t = ^pthread_t;
  pthread_t = dword;

{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

Type
  TPthreadFastlock = _pthread_fastlock;
  PPthreadFastlock = ^TPthreadFastlock;

  TCondVar = pthread_cond_t;
  PCondVar = ^TCondVar;

  TPthreadCondattr = pthread_condattr_t;
  PPthreadCondattr = ^TPthreadCondattr;

  TPthreadMutex = pthread_mutex_t;
  PPthreadMutex = ^TPthreadMutex;
  TRTLCriticalSection = TPthreadMutex;
  PRTLCriticalSection = ^TRTLCriticalSection;

  TMutexAttribute = pthread_mutexattr_t;
  PMutexAttribute = ^TMutexAttribute;

  TPThreadOnce = pthread_once_t;
  PPThreadOnce = ^pthread_once_t;

  TPthreadRWlock = pthread_rwlock_t;
  PPthreadRWlock = ^TPthreadRWlock;

  TPthreadRWlockAttribute = pthread_rwlockattr_t;
  PPthreadRWlockAttribute = ^TPthreadRWlockAttribute;

  TPthreadSpinlock = pthread_spinlock_t;
  PPthreadSpinlock = ^TPthreadSpinlock;

  TPthreadBarrier = pthread_barrier_t;
  PPthreadBarrier = ^TPthreadBarrier;

  TPthreadBarrierAttribute = pthread_barrierattr_t;
  PPthreadBarrierAttribute = ^TPthreadBarrierAttribute;

  TThreadID = pthread_t;
  TPThreadKey = pthread_key_t;
    

