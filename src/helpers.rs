/// Quick bitset implementation
#[derive(Clone)]
pub struct BitSet {
    v: Vec<u32>,
}
impl BitSet {
    pub fn new(count: usize) -> Self {
        BitSet { v: vec![0; (count + 31) / 32] }
    }
    pub fn cap(&self) -> usize {
        self.v.len() * 32
    }
    pub fn is_set(&self, slot: usize) -> bool {
        if slot >= self.v.len() * 32 {
            false
        }
        else {
            self.v[slot / 32] & (1 << (slot % 32)) != 0
        }
    }
    /// Returns the previous state of the entry
    pub fn set(&mut self, slot: usize) -> bool {
        let rv = self.is_set(slot);
        self.v[slot / 32] |= 1 << (slot % 32);
        rv
    }
    /// Returns the previous state of the entry
    pub fn clear(&mut self, slot: usize) -> bool {
        let rv = self.is_set(slot);
        self.v[slot / 32] &= !(1 << (slot % 32));
        rv
    }
}
