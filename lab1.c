#include <stdio.h>
#include <stdlib.h>

typedef struct node {
    char data;
    struct node *next;
} Node;

// Додавання елементи до списку
void add(Node **head, char data) {
    Node *new_node = malloc(sizeof(Node));
    new_node->data = data;
    new_node->next = *head;
    *head = new_node;
}

// Видалення списку
void deleteList(Node **head) {
    Node *node = *head;
    while (node != NULL) {
        Node *next = node->next;
        free(node);
        node = next;
    }
    *head = NULL;
}

// Виведення списку в консоль
void printList(Node *head) {
    printf("{ ");
    Node *node = head;
    while (node != NULL) {
        printf("%c ", node->data);
        node = node->next;
    }
    printf("}\n");
}

// Функція різниці двох списків
void difference(Node *set1, Node *set2, Node **result) {
    Node *node1 = set1;
    while (node1 != NULL) {
        Node *node2 = set2;
        int found = 0;
        while (node2 != NULL) {
            if (node1->data == node2->data) {
                found = 1;
                break;
            }
            node2 = node2->next;
        }
        if (!found) {
            add(result, node1->data);
        }
        node1 = node1->next;
    }
}

// Введення лементів списку
void inputList(Node** head_ref) {
    int n;
    char data;
    printf("Введіть кількість елементів: ");
    if (scanf("%d", &n) != 1) {
        printf("Помилка! Ви ввели не число");
        exit(1);
    }
    else if (n < 1) {
        printf("Помилка! Ви ввели число менше за 1");
        exit(1);
    }

    printf("Введіть елементи списку (через пробіл або як однією строкою): ");
    for (int i = 0; i < n; i++) {
        scanf(" %c", &data);
        add(head_ref, data);
    }

    data = getchar();
    if (data != '\n') {
        printf("Помилка! Ви ввели завелику кількість символів");
        exit(1);
    }
    printf("\n");
}

int main() {
    printf("Дітріх А. О. Лабораторна робота №1, Варіант 8");
    printf("\nСкласти процедуру отримання різниці множин символів\nМножини моделюються списком\n\n");

    Node *set1 = NULL;
    Node *set2 = NULL;
    Node *result = NULL;

    inputList(&set1);
    inputList(&set2);
    difference(set1, set2, &result);

    printf("Список 1:\n");
    printList(set1);
    printf("\nСписок 2:\n");
    printList(set2);
    printf("\nРізниця списків:\n");
    printList(result);

    deleteList(&set1);
    deleteList(&set2);
    deleteList(&result);
    return 0;
}