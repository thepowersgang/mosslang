
//use crate::INDENT;

pub struct IterableTrie<T> {
    /// Number of root entries, must be >0  if `storage` is non-empty
    num_roots: usize,
    /// Is the empty list in this trie
    #[allow(dead_code)]
    has_empty: bool,
    /// Linear storage for tree
    storage: Vec<Ent<T>>,
}
impl<T> IterableTrie<T>
where
    //T: ::std::fmt::Debug,
{
    pub fn build_from_list(list_of_lists: &[impl AsRef<[T]>]) -> Self
    where
        T: Copy + PartialOrd + PartialEq,
    {
        assert!( list_of_lists.is_sorted_by_key(|v| v.as_ref()) );

        fn fill_recurse<T, L>(storage: &mut Vec<Ent<T>>, idx: usize, mut list_of_lists: &[L]) -> (bool,usize)
        where
            L: AsRef<[T]>,
            T: PartialEq,
            T: Copy,
            //T: ::std::fmt::Debug,
        {
            let mut n_direct = 0;
            while let Some((true, tail)) = list_of_lists.split_first().map(|(v,t)| (v.as_ref().len() <= idx, t)) {
                list_of_lists = tail;
                n_direct += 1;
            }

            let parent_slot = storage.len().wrapping_sub(1);
            let mut child_idx = 0;
            for (v, s, e) in UniqueRanges::new(list_of_lists, idx) {
                // Create an entry for this range
                let i = storage.len();
                storage.push(Ent::new(ParentIndex::new(parent_slot, child_idx), *v));
                let (is_end, n_children) = fill_recurse(storage, idx+1, &list_of_lists[s..e]);
                storage[i].total_len = storage.len() - i;
                storage[i].n_children = n_children;
                storage[i].is_end = is_end;
                child_idx += 1;
            }
            (n_direct > 0, child_idx)
        }

        let mut storage = Vec::new();
        let (has_empty, num_roots) = fill_recurse(&mut storage, 0, list_of_lists);
        IterableTrie {
            num_roots,
            has_empty,
            storage,
        }
    }
    /// Total number of nodes in the trie
    pub fn total_size(&self) -> usize {
        self.storage.len()
    }
    /// Visit every entry in the trie
    pub fn visit(&self, mut visitor: impl FnMut(VisitInfo, &T)->VisitRes) {
        //let mut tree_index = 0;
        let mut depth = 0;
        let mut i = 0;
        loop {
            if i >= self.storage.len() {
                return;
            }
            let e = &self.storage[i];
            //println!("{:w$}#{} {:?}", "", i, e, w=depth as usize);
            let visit_children = match visitor(VisitInfo { depth, is_leaf: e.is_end }, &e.value) {
                VisitRes::Continue => true,
                VisitRes::StopArm => false,
                VisitRes::StopAll => return,
                };
            // If there are children, recurse into them
            if visit_children && e.n_children > 0 {
                i += 1;
                depth += 1;
            }
            else {
                loop {
                    let e = &self.storage[i];
                    //println!("{:w$}>>#{} {:?}", "", i, e, w=depth as usize);
                    // No children, back out to the parent and try the next sibling
                    if let Some(next_i) = self.next_child(i) {
                        i = next_i;
                        // Depth stays the same
                        //tree_index += 1;
                        break;
                    }
                    // No siblings, back out to parent and look at their siblings
                    let Some(next_i) = e.parent_index.index() else {
                        // We've reached the root, return
                        return ;
                        };
                    depth -= 1;
                    i = next_i;
                }
            }
        }
    }

    fn next_child(&self, cur_idx: usize) -> Option<usize> {
        let next = cur_idx + self.storage[cur_idx].total_len;
        let n_children = match self.storage[cur_idx].parent_index.index() {
            Some(i) => self.storage[i].n_children,
            None => self.num_roots,
        };
        if self.storage[cur_idx].parent_index.child_num() + 1 >= n_children {
            None
        }
        else {
            Some(next)
        }
    }
}
pub struct VisitInfo {
    #[allow(dead_code)]
    pub depth: u32,
    #[allow(dead_code)]
    pub is_leaf: bool,
}
#[allow(dead_code)]
pub enum VisitRes {
    /// Continue visiting this arm and all others
    Continue,
    /// Stop visiting this arm (don't visit children)
    StopArm,
    /// Stop visiting all arms, return from `visit` immediately
    StopAll,
}
#[derive(Debug)]
struct Ent<T> {
    /// Index of the parent node
    parent_index: ParentIndex,
    /// Total number of entries covered by this entry (itself and all descendants)
    total_len: usize,
    /// Number of direct children - direct children come in the next available slot
    n_children: usize,
    /// Number of empties
    is_end: bool,
    /// Contained value
    value: T,
}
impl<T> Ent<T> {
    fn new(parent_index: ParentIndex, value: T) -> Self { 
        Ent {
            parent_index,
            total_len: 0,
            n_children: 0,
            is_end: false,
            value,
        }
    }
}
#[derive(Debug)]
struct ParentIndex {
    /// if !0, this is a root
    index: [u8; 3],
    child_num: [u8; 1],
}
impl ParentIndex {
    fn new(index: usize, child_num: usize) -> Self {
        assert!(child_num <= 255);
        let i = index.to_le_bytes();
        ParentIndex {
            index: [i[0], i[1], i[2]],
            child_num: [child_num as u8],
        }
    }
    fn index(&self) -> Option<usize> {
        if self.index == [!0; 3] {
            None
        }
        else {
            Some( u32::from_le_bytes([self.index[0], self.index[1], self.index[2], 0]) as usize )
        }
    }
    fn child_num(&self) -> usize {
        self.child_num[0].into()
    }
}



/// Iterate over unique sub-ranges of a list
struct UniqueRanges<'a, T: 'a, L> {
    inner_idx: usize,
    list: ::std::slice::Iter<'a, L>,
    cur: Option<&'a T>,
    start_idx: usize,
    end_idx: usize,
}
impl<'a, T: 'a, L> UniqueRanges<'a, T,L>
where
    L: AsRef<[T]>,
    T: PartialEq,
{
    fn new(list_of_lists: &'a [L], inner_idx: usize) -> Self {
        Self {
            inner_idx,
            list: list_of_lists.iter(),
            cur: None,
            start_idx: 0,
            end_idx: 0,
        }
    }
}
impl<'a, T: 'a, L> Iterator for UniqueRanges<'a, T,L>
where
    L: AsRef<[T]>,
    T: PartialEq,
{
    type Item = (&'a T,usize,usize);

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let v = self.list.next().map(|v| &v.as_ref()[self.inner_idx]);
            match self.cur {
            None => {
                // This is the start/end case. We rely on `list` being fused for this logic
                if v.is_none() {
                    return None;
                }
                self.cur = v;
                self.end_idx += 1;
            }
            Some(cur) if Some(cur) == v => {
                self.end_idx += 1;
            },
            Some(cur) => {
                let s = self.start_idx;
                let e = self.end_idx;
                self.start_idx = self.end_idx;
                self.end_idx += 1;
                
                let rv = (cur, s, e);
                self.cur = v;
                return Some(rv);
                },
            }
        }
    }
}
mod tests {
    #[test]
    fn unique_ranges_simple() {
        let lists = [
            &[1, 2, 3, 4] as &[_],
            &[1, 2, 3, 5],
            &[1, 2, 3, 6],
            &[1, 2, 3, 6, 1],
            &[1, 2, 3, 7],
        ];
        let mut it = super::UniqueRanges::new(&lists, 3);
        assert_eq!(it.next(), Some((&4, 0, 1)));
        assert_eq!(it.next(), Some((&5, 1, 2)));
        assert_eq!(it.next(), Some((&6, 2, 4)));
        assert_eq!(it.next(), Some((&7, 4, 5)));
    }

    #[test]
    fn trie_simple() {
        let lists = [
            &[1, 2, 3, 4] as &[_],
            &[1, 2, 3, 5],
            &[1, 2, 3, 5, 1],
            &[1, 2, 3, 6],
            &[1, 2, 3, 6, 1],
            &[1, 2, 3, 7],
            &[2, 3, 7],
        ];

        let mut expected_visit = [
            1, 2, 3, 4,
            5, 1,
            6, 1, 7,
            2, 3, 7,
        ].iter();
        let trie = super::IterableTrie::build_from_list(&lists);
        trie.visit(|_vi, value| {
            assert_eq!(Some(value), expected_visit.next());
            super::VisitRes::Continue
        });
        assert_eq!(None, expected_visit.next());
    }
}