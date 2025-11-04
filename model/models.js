sap.ui.define([
    "sap/ui/model/json/JSONModel"
], function(JSONModel) {
    "use strict";

    return {
        createMessageModel: function() {
            var oModel = new JSONModel({
                messages: [
                    {
                        sender: "Пользователь 1",
                        text: "Привет! Это первое сообщение в чате.",
                        timestamp: "2024-01-15 10:30",
                        icon: "sap-icon://employee",
                        Actions: [
                            { Text: "Action 1", Key: "action1" },
                            { Text: "Action 2", Key: "action2" }
                        ]
                    },
                    {
                        sender: "Пользователь 2",
                        text: "Здравствуйте! Как дела?",
                        timestamp: "2024-01-15 10:32",
                        icon: "sap-icon://customer",
                        Actions: [
                            { Text: "Action 1", Key: "action1" },
                            { Text: "Action 2", Key: "action2" }
                        ]
                    },
                    {
                        sender: "Пользователь 1",
                        text: "Отлично, спасибо! А у вас?",
                        timestamp: "2024-01-15 10:35",
                        icon: "sap-icon://employee",
                        Actions: [
                            { Text: "Action 1", Key: "action1" },
                            { Text: "Action 2", Key: "action2" }
                        ]
                    },
                    {
                        sender: "Система",
                        text: "Добрый день! Рад всех видеть в этом чате.",
                        timestamp: "2024-01-15 10:40",
                        icon: "sap-icon://robot",
                        Actions: [
                            { Text: "Action 1", Key: "action1" },
                            { Text: "Action 2", Key: "action2" }
                        ]
                    },
                    {
                        sender: "Пользователь 2",
                        text: "Спасибо! Всё прекрасно, работаем над новым проектом.",
                        timestamp: "2024-01-15 10:42",
                        icon: "sap-icon://customer",
                        Actions: [
                            { Text: "Action 1", Key: "action1" },
                            { Text: "Action 2", Key: "action2" }
                        ]
                    },
                    {
                        sender: "Пользователь 1",
                        text: "Интересно! Можете рассказать подробнее?",
                        timestamp: "2024-01-15 10:45",
                        icon: "sap-icon://employee",
                        Actions: [
                            { Text: "Action 1", Key: "action1" },
                            { Text: "Action 2", Key: "action2" }
                        ]
                    },
                    {
                        sender: "Пользователь 4",
                        text: "Присоединяюсь к беседе. Какая тема обсуждается?",
                        timestamp: "2024-01-15 10:50",
                        icon: "sap-icon://group",
                        Actions: [
                            { Text: "Action 1", Key: "action1" },
                            { Text: "Action 2", Key: "action2" }
                        ]
                    },
                    {
                        sender: "Система",
                        text: "Обсуждаем новые проекты и планы на будущее.",
                        timestamp: "2024-01-15 10:52",
                        icon: "sap-icon://cloud",
                        Actions: [
                            { Text: "Action 1", Key: "action1" },
                            { Text: "Action 2", Key: "action2" }
                        ]
                    },
                    {
                        sender: "Пользователь 2",
                        text: "Да, это очень важная тема. Нужно скоординировать действия.",
                        timestamp: "2024-01-15 10:55",
                        icon: "sap-icon://customer",
                        Actions: [
                            { Text: "Action 1", Key: "action1" },
                            { Text: "Action 2", Key: "action2" }
                        ]
                    },
                    {
                        sender: "Пользователь 1",
                        text: "Отлично! Давайте назначим встречу для обсуждения деталей.",
                        timestamp: "2024-01-15 11:00",
                        icon: "sap-icon://employee",
                        Actions: [
                            { Text: "Action 1", Key: "action1" },
                            { Text: "Action 2", Key: "action2" }
                        ]
                    }
                ]
            });
            return oModel;
        }
    };
});

