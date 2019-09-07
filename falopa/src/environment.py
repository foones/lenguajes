
class Environment:

    def __init__(self):
        self._ribs = [{}]

    def define(self, name, value):
        self._ribs[-1][name] = value

    def is_locally_defined(self, name):
        return name in self._ribs[-1]

    def local_value(self, name):
        assert self.is_locally_defined(name)
        return self._ribs[-1][name]

    def is_defined(self, name):
        for i in reversed(range(len(self._ribs))):
            if name in self._ribs[i]:
                return True
        return False

    def value(self, name):
        assert self.is_defined(name)
        for i in reversed(range(len(self._ribs))):
            if name in self._ribs[i]:
                return self._ribs[i][name]

    def open_scope(self):
        self._ribs.append({})

    def close_scope(self):
        self._ribs.pop()

    def current_scope(self):
        return self._ribs[-1]

    def all_values_in_parent_scopes(self):
        values = set()
        for rib in self._ribs[:-1]:
            values |= set(rib.values())
        return values

class PersistentEnvironment:

    def __init__(self, parent=None):
        self._rib = {}
        self._parent = parent

    def define(self, name, value):
        self._rib[name] = value

    def is_defined(self, name):
        env = self
        while env is not None:
            if name in env._rib:
                return True
            env = env._parent
        return False

    def set(self, name, value):
        env = self
        while env is not None:
            if name in env._rib:
                env._rib[name] = value
                return
            env = env._parent

    def value(self, name):
        env = self
        while env is not None:
            if name in env._rib:
                return env._rib[name]
            env = env._parent
        raise Exception('Name "{name}" not in environment.'.format(name=name))

    def extended(self):
        return PersistentEnvironment(parent=self)

    def parent(self):
        return self._parent

