#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
pub struct DebruijnIndex {
    depth: u32,
}

impl DebruijnIndex {
    pub const ONE: DebruijnIndex = DebruijnIndex::new(1);
    pub const INNERMOST: DebruijnIndex = DebruijnIndex::new(0);

    #[must_use]
    pub const fn new(depth: u32) -> Self {
        DebruijnIndex { depth }
    }

    #[must_use]
    pub const fn depth(self) -> u32 {
        self.depth
    }

    #[must_use]
    pub const fn within(self, outer_binder: DebruijnIndex) -> bool {
        self.depth < outer_binder.depth
    }

    #[must_use]
    pub const fn shifted_in_by(self, amount: DebruijnIndex) -> DebruijnIndex {
        DebruijnIndex::new(self.depth + amount.depth)
    }

    #[must_use]
    pub const fn shifted_out_by(self, amount: DebruijnIndex) -> Option<DebruijnIndex> {
        if self.within(amount) {
            None
        } else {
            Some(DebruijnIndex::new(self.depth - amount.depth))
        }
    }

    #[must_use]
    pub const fn shifted_out(self) -> Option<DebruijnIndex> {
        self.shifted_out_by(DebruijnIndex::ONE)
    }

    #[must_use]
    pub const fn shifted_in(self) -> DebruijnIndex {
        self.shifted_in_by(DebruijnIndex::ONE)
    }

    pub fn shift_in(&mut self) {
        *self = self.shifted_in();
    }

    pub fn shift_out(&mut self) {
        *self = self.shifted_out().unwrap();
    }
}
