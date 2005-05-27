template<class T, class K> class SList;

template<class T, class K>
class SListNode
{
        friend class SList<T, K>;

        public:

                SListNode( T entry, K Key );
                virtual ~SListNode();


        private:

                K m_key;
                T m_pEntry;

                SListNode<T, K> *m_pNext;

}; // SListNode


/* public */
template<class T, class K>
SListNode<T, K>::SListNode ( T entry, K key )
    : m_key (key),
      m_pNext (NULL),
      m_pEntry (entry)
{
} // ctor

/* public */
template<class T, class K>
SListNode<T, K>::~SListNode()
{
    if (m_pEntry != NULL) {
        delete m_pEntry;
        m_pEntry = NULL;
        }

} // dtor

template<class T, class K>
class SList
{
        public:

        SList();
        virtual ~SList();

                SList<T, K>( const SList<T, K> &source );
                SList<T, K> &operator=( const SList<T, K> &source );


        public:

                void Dump();

        BOOL IsEmpty();
        T Lookup( K key );
        void DelEntry( K key );
        void AddEntry( T entry, K key );

        //
        // getters
        //
        T Head();
        T Tail();
        T Entry();
        ULONG Count();

        //
        // intrinsic iteration
        //
                void Next();
        void Reset();
                BOOL AtEnd();
                BOOL AtStart();


        private:

        ULONG m_count;

            SListNode<T, K> *m_pHead;
        SListNode<T, K> *m_pTail;
        SListNode<T, K> *m_pCursor;

        CRITICAL_SECTION m_criticalSection;

}; // SList

/* public */
template<class T, class K>
SList<T, K>::SList()
    : m_count (0),
      m_pHead (NULL),
      m_pTail (NULL),
      m_pCursor (NULL)
{
    InitializeCriticalSection (&m_criticalSection);
} // ctor

/* public */
template<class T, class K>
SList<T, K>::~SList()
{
    if (m_pHead != NULL) {
        Synchronize guard (m_criticalSection);

        SListNode<T, K> *cursor = m_pHead;

        // delete all entries
        m_count = 0;
        for (; ((m_pHead = m_pHead->m_pNext) != NULL); cursor = m_pHead)
            delete cursor;
        delete cursor;
        }

    DeleteCriticalSection (&m_criticalSection);
} // dtor

/* public */
template<class T, class K>
SList<T, K> &SList<T, K>::operator= (const SList<T, K> &source)
{
    Synchronize guard (m_criticalSection);

    memcpy (this, &source, sizeof (SList<T, K>));

    return *this;

} // assignment operator

/* public */
template<class T, class K>
SList<T, K>::SList (const SList<T, K> &source)
{
    Synchronize guard (m_criticalSection);

    m_pHead = source.m_pHead;
    m_pTail = source.m_pTail;
} // copy ctor

/* public */
template<class T, class K>
void SList<T, K>::Dump()
{
    Synchronize guard (m_criticalSection);

    SListNode<T, K> *cursor = m_pHead;

    for (; (cursor != NULL); cursor = cursor->m_pNext)
        cursor->m_pEntry->Dump();

} // SList<T, K>::Dump

/* public */
template<class T, class K>
ULONG SList<T, K>::Count()
{
    Synchronize guard (m_criticalSection);
    return m_count;

} // SList<T, K>::Count

/* public */
template<class T, class K>
void SList<T, K>::AddEntry (T entry, K key)
{
    Synchronize guard (m_criticalSection);

    ++m_count;
    SListNode<T, K> * temp = new SListNode<T, K> (entry, key);

    // list insertion implies appending to the end of the list
    if (m_pHead == NULL)
        m_pHead = temp;

    else
        m_pTail->m_pNext = temp;

    m_pCursor = m_pTail = temp;

} // SList<T, K>::AddEntry

/* public */
template<class T, class K>
void SList<T, K>::DelEntry (K key)
{

    Synchronize guard (m_criticalSection);

    if (m_pHead != NULL) {

        SListNode<T, K> *cursor = m_pHead;

        //
        // Case 1: delete head entry
        //
        if (m_pHead->m_pEntry->Compare (key) == TRUE) {
            --m_count;

            // consider case where head == tail
            if ((m_pHead = m_pHead->m_pNext) == NULL)
                m_pCursor = m_pTail = m_pHead;

            else
                m_pCursor = m_pHead;


            delete cursor;
            cursor = NULL;
            }
        //
        // Case 2: delete inner entry
        //
        else {
            SListNode<T, K> *precursor = cursor;


            // scan for match
            for (; ((cursor = cursor->m_pNext) != NULL); precursor = cursor) {
                if (cursor->m_pEntry->Compare( key ) == TRUE) {
                    --m_count;
                    m_pCursor = precursor;
                    precursor->m_pNext = cursor->m_pNext;

                    // consider case where deleted entry is the tail
                    if (m_pTail == cursor) {
                        m_pTail = precursor;
                        m_pTail->m_pNext = NULL;
                        }

                    delete cursor;
                    cursor = NULL;
                    break;
                    }

                } // for
            }
        }
} // SList<T, K>::DelEntry


/* public */
template<class T, class K>
T SList<T, K>::Lookup (K key)
{
    Synchronize guard (m_criticalSection);

    SListNode<T, K> *cursor = m_pHead;

    // scan for match
    for ( ;
          ((cursor != NULL) && (cursor->m_pEntry->Compare (key) == FALSE));
          cursor = cursor->m_pNext )
        ; // continue


    return ((cursor != NULL) ? cursor->m_pEntry : NULL);
} // SList<T, K>::Lookup

/* public */
template<class T, class K>
T SList<T, K>::Entry()
{
    Synchronize guard (m_criticalSection);

    return ((m_pCursor != NULL) ? m_pCursor->m_pEntry : NULL);

} // SList<T, K>::Entry

/* public */
template<class T, class K>
T SList<T, K>::Head()
{
    Synchronize guard (m_criticalSection);

    return ((m_pHead != NULL) ? m_pHead->m_pEntry : NULL);
} // SList<T, K>::Head

/* public */
template<class T, class K>
T SList<T, K>::Tail()
{
    Synchronize guard (m_criticalSection);

    return ((m_pTail != NULL) ? m_pTail->m_pEntry : NULL);

} // SList<T, K>::Tail

/* public */
template<class T, class K>
void SList<T, K>::Next()
{
    Synchronize guard (m_criticalSection);

    if (m_pCursor != NULL)
        m_pCursor = m_pCursor->m_pNext;

} // SList<T, K>::Next()

/* public */
template<class T, class K>
void SList<T, K>::Reset()
{
    Synchronize guard (m_criticalSection);

    m_pCursor = m_pHead;

} // SList<T, K>::Reset

/* public */
template<class T, class K>
BOOL SList<T, K>::AtEnd()
{
    Synchronize guard (m_criticalSection);

    return (BOOL)(m_pCursor == NULL);
} // SList<T, K>::AtEnd

/* public */
template<class T, class K>
BOOL SList<T, K>::AtStart()
{
    Synchronize guard (m_criticalSection);

    return (BOOL)(m_pCursor == m_pHead);
} // SList<T, K>::AtStart

/* public */
template<class T, class K>
BOOL SList<T, K>::IsEmpty()
{
    Synchronize guard (m_criticalSection);

    return (BOOL)(m_pHead == NULL);
} // SList<T, K>::IsEmpty
