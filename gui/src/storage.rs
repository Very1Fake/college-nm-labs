#[derive(Default)]
pub struct Storage<T: Default> {
    pub content: Vec<T>,
    pub selected: Option<usize>,
}

impl<T: Default> Storage<T> {
    pub fn get_selected(&self) -> Option<&T> {
        self.selected.map(|i| &self.content[i])
    }

    pub fn remove(&mut self, id: usize) {
        if id < self.content.len() {
            self.content.remove(id);
            self.content.shrink_to_fit(); // Clean up

            if let Some(i) = self.selected {
                if self.content.get(i).is_none() {
                    self.selected = None;
                }
            }
        }
    }
}

impl<T: Default> From<Vec<T>> for Storage<T> {
    fn from(content: Vec<T>) -> Self {
        Self {
            content,
            ..Default::default()
        }
    }
}
