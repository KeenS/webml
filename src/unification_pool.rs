#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub struct NodeId(usize);

#[derive(Debug, Clone)]
enum Node<T> {
    Value(T),
    Refer(NodeId),
}

impl<T> Node<T> {
    fn new(t: T) -> Self {
        Node::Value(t)
    }

    fn refer(node_id: NodeId) -> Self {
        Node::Refer(node_id)
    }

    fn take(&mut self) -> Option<T> {
        let node = std::mem::replace(self, Node::Refer(NodeId(std::usize::MAX)));
        match node {
            Node::Value(t) => Some(t),
            Node::Refer(_) => None,
        }
    }
}

#[derive(Debug)]
pub struct UnificationPool<T> {
    pool: Vec<Node<T>>,
}

impl<T> UnificationPool<T> {
    pub fn new() -> Self {
        Self { pool: vec![] }
    }

    fn register(&mut self, node: Node<T>) -> NodeId {
        self.pool.push(node);
        NodeId(self.pool.len() - 1)
    }

    pub fn node_new(&mut self, t: T) -> NodeId {
        self.register(Node::new(t))
    }

    fn at(&self, node_id: NodeId) -> &Node<T> {
        &self.pool[node_id.0]
    }

    fn at_mut(&mut self, node_id: NodeId) -> &mut Node<T> {
        &mut self.pool[node_id.0]
    }

    fn value_id(&self, mut id: NodeId) -> NodeId {
        loop {
            match self.at(id) {
                Node::Value(_) => return id,
                Node::Refer(new_id) => id = *new_id,
            }
        }
    }

    pub fn value_of(&self, mut id: NodeId) -> &T {
        loop {
            match self.at(id) {
                Node::Value(t) => return t,
                Node::Refer(new_id) => id = *new_id,
            }
        }
    }

    fn reduction(&mut self, mut start: NodeId) {
        let value_id = self.value_id(start);
        loop {
            match self.at_mut(start) {
                Node::Value(_) => {
                    return;
                }
                Node::Refer(ref mut id) => {
                    start = *id;
                    *id = value_id;
                }
            }
        }
    }

    pub fn try_unify_with<E>(
        &mut self,
        id1: NodeId,
        id2: NodeId,
        try_unify: impl FnOnce(&mut Self, T, T) -> Result<T, E>,
    ) -> Result<NodeId, E> {
        let lid = self.value_id(id1);
        let rid = self.value_id(id2);
        if lid == rid {
            return Ok(lid);
        }
        let l = self.at_mut(lid).take().unwrap();
        let r = self.at_mut(rid).take().unwrap();
        let new = try_unify(self, l, r)?;
        *self.at_mut(lid) = Node::Value(new);
        *self.at_mut(rid) = Node::Refer(lid);

        self.reduction(id1);
        self.reduction(id2);
        Ok(lid)
    }
}
